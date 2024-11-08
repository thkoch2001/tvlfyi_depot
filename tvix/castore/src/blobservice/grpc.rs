use super::{BlobReader, BlobService, BlobWriter, ChunkedReader};
use crate::composition::{CompositionContext, ServiceBuilder};
use crate::{
    proto::{self, stat_blob_response::ChunkMeta},
    B3Digest,
};
use futures::sink::SinkExt;
use std::{
    io::{self, Cursor},
    pin::pin,
    sync::Arc,
    task::Poll,
};
use tokio::io::AsyncWriteExt;
use tokio::task::JoinHandle;
use tokio_stream::{wrappers::ReceiverStream, StreamExt};
use tokio_util::{
    io::{CopyToBytes, SinkWriter},
    sync::PollSender,
};
use tonic::{async_trait, Code, Status};
use tracing::{instrument, Instrument as _};

/// Connects to a (remote) tvix-store BlobService over gRPC.
#[derive(Clone)]
pub struct GRPCBlobService<T> {
    instance_name: String,
    /// The internal reference to a gRPC client.
    /// Cloning it is cheap, and it internally handles concurrent requests.
    grpc_client: proto::blob_service_client::BlobServiceClient<T>,
}

impl<T> GRPCBlobService<T> {
    /// construct a [GRPCBlobService] from a [proto::blob_service_client::BlobServiceClient].
    pub fn from_client(
        instance_name: String,
        grpc_client: proto::blob_service_client::BlobServiceClient<T>,
    ) -> Self {
        Self {
            instance_name,
            grpc_client,
        }
    }
}

#[async_trait]
impl<T> BlobService for GRPCBlobService<T>
where
    T: tonic::client::GrpcService<tonic::body::BoxBody> + Send + Sync + Clone + 'static,
    T::ResponseBody: tonic::codegen::Body<Data = tonic::codegen::Bytes> + Send + 'static,
    <T::ResponseBody as tonic::codegen::Body>::Error: Into<tonic::codegen::StdError> + Send,
    T::Future: Send,
{
    #[instrument(skip(self, digest), fields(blob.digest=%digest, instance_name=%self.instance_name))]
    async fn has(&self, digest: &B3Digest) -> io::Result<bool> {
        match self
            .grpc_client
            .clone()
            .stat(proto::StatBlobRequest {
                digest: digest.clone().into(),
                ..Default::default()
            })
            .await
        {
            Ok(_blob_meta) => Ok(true),
            Err(e) if e.code() == Code::NotFound => Ok(false),
            Err(e) => Err(io::Error::new(io::ErrorKind::Other, e)),
        }
    }

    #[instrument(skip(self, digest), fields(blob.digest=%digest, instance_name=%self.instance_name), err)]
    async fn open_read(&self, digest: &B3Digest) -> io::Result<Option<Box<dyn BlobReader>>> {
        // First try to get a list of chunks. In case there's only one chunk returned,
        // buffer its data into a Vec, otherwise use a ChunkedReader.
        // We previously used NaiveSeeker here, but userland likes to seek backwards too often,
        // and without store composition this will get very noisy.
        // FUTUREWORK: use CombinedBlobService and store composition.
        match self.chunks(digest).await {
            Ok(None) => Ok(None),
            Ok(Some(chunks)) => {
                if chunks.is_empty() || chunks.len() == 1 {
                    // No more granular chunking info, treat this as an individual chunk.
                    // Get a stream of [proto::BlobChunk], or return an error if the blob
                    // doesn't exist.
                    return match self
                        .grpc_client
                        .clone()
                        .read(proto::ReadBlobRequest {
                            digest: digest.clone().into(),
                        })
                        .await
                    {
                        Ok(stream) => {
                            let data_stream = stream.into_inner().map(|e| {
                                e.map(|c| c.data)
                                    .map_err(|s| std::io::Error::new(io::ErrorKind::InvalidData, s))
                            });

                            // Use StreamReader::new to convert to an AsyncRead.
                            let mut data_reader = tokio_util::io::StreamReader::new(data_stream);

                            let mut buf = Vec::new();
                            // TODO: only do this up to a certain limit.
                            tokio::io::copy(&mut data_reader, &mut buf).await?;

                            Ok(Some(Box::new(Cursor::new(buf))))
                        }
                        Err(e) if e.code() == Code::NotFound => Ok(None),
                        Err(e) => Err(io::Error::new(io::ErrorKind::Other, e)),
                    };
                }

                // The chunked case. Let ChunkedReader do individual reads.
                // TODO: we should store the chunking data in some local cache,
                // so `ChunkedReader` doesn't call `self.chunks` *again* for every chunk.
                // Think about how store composition will fix this.
                let chunked_reader = ChunkedReader::from_chunks(
                    chunks.into_iter().map(|chunk| {
                        (
                            chunk.digest.try_into().expect("invalid b3 digest"),
                            chunk.size,
                        )
                    }),
                    Arc::new(self.clone()) as Arc<dyn BlobService>,
                );
                Ok(Some(Box::new(chunked_reader)))
            }
            Err(e) => Err(e)?,
        }
    }

    /// Returns a BlobWriter, that'll internally wrap each write in a
    /// [proto::BlobChunk], which is send to the gRPC server.
    #[instrument(skip_all, fields(instance_name=%self.instance_name))]
    async fn open_write(&self) -> Box<dyn BlobWriter> {
        // set up an mpsc channel passing around Bytes.
        let (tx, rx) = tokio::sync::mpsc::channel::<bytes::Bytes>(10);

        // bytes arriving on the RX side are wrapped inside a
        // [proto::BlobChunk], and a [ReceiverStream] is constructed.
        let blobchunk_stream = ReceiverStream::new(rx).map(|x| proto::BlobChunk { data: x });

        // spawn the gRPC put request, which will read from blobchunk_stream.
        let task = tokio::spawn({
            let mut grpc_client = self.grpc_client.clone();
            async move { Ok::<_, Status>(grpc_client.put(blobchunk_stream).await?.into_inner()) }
                // instrument the task with the current span, this is not done by default
                .in_current_span()
        });

        // The tx part of the channel is converted to a sink of byte chunks.
        let sink = PollSender::new(tx)
            .sink_map_err(|e| std::io::Error::new(std::io::ErrorKind::BrokenPipe, e));

        // … which is turned into an [tokio::io::AsyncWrite].
        let writer = SinkWriter::new(CopyToBytes::new(sink));

        Box::new(GRPCBlobWriter {
            task_and_writer: Some((task, writer)),
            digest: None,
        })
    }

    #[instrument(skip(self, digest), fields(blob.digest=%digest, instance_name=%self.instance_name), err)]
    async fn chunks(&self, digest: &B3Digest) -> io::Result<Option<Vec<ChunkMeta>>> {
        let resp = self
            .grpc_client
            .clone()
            .stat(proto::StatBlobRequest {
                digest: digest.clone().into(),
                send_chunks: true,
                ..Default::default()
            })
            .await;

        match resp {
            Err(e) if e.code() == Code::NotFound => Ok(None),
            Err(e) => Err(io::Error::new(io::ErrorKind::Other, e)),
            Ok(resp) => {
                let resp = resp.into_inner();

                resp.validate()
                    .map_err(|e| std::io::Error::new(io::ErrorKind::InvalidData, e))?;

                Ok(Some(resp.chunks))
            }
        }
    }
}

#[derive(serde::Deserialize, Debug)]
#[serde(deny_unknown_fields)]
pub struct GRPCBlobServiceConfig {
    url: String,
}

impl TryFrom<url::Url> for GRPCBlobServiceConfig {
    type Error = Box<dyn std::error::Error + Send + Sync>;
    fn try_from(url: url::Url) -> Result<Self, Self::Error> {
        //   normally grpc+unix for unix sockets, and grpc+http(s) for the HTTP counterparts.
        // - In the case of unix sockets, there must be a path, but may not be a host.
        // - In the case of non-unix sockets, there must be a host, but no path.
        // Constructing the channel is handled by tvix_castore::channel::from_url.
        Ok(GRPCBlobServiceConfig {
            url: url.to_string(),
        })
    }
}

#[async_trait]
impl ServiceBuilder for GRPCBlobServiceConfig {
    type Output = dyn BlobService;
    async fn build<'a>(
        &'a self,
        instance_name: &str,
        _context: &CompositionContext,
    ) -> Result<Arc<dyn BlobService>, Box<dyn std::error::Error + Send + Sync + 'static>> {
        let client = proto::blob_service_client::BlobServiceClient::new(
            crate::tonic::channel_from_url(&self.url.parse()?).await?,
        );
        Ok(Arc::new(GRPCBlobService::from_client(
            instance_name.to_string(),
            client,
        )))
    }
}

pub struct GRPCBlobWriter<W: tokio::io::AsyncWrite> {
    /// The task containing the put request, and the inner writer, if we're still writing.
    task_and_writer: Option<(JoinHandle<Result<proto::PutBlobResponse, Status>>, W)>,

    /// The digest that has been returned, if we successfully closed.
    digest: Option<B3Digest>,
}

#[async_trait]
impl<W: tokio::io::AsyncWrite + Send + Sync + Unpin + 'static> BlobWriter for GRPCBlobWriter<W> {
    async fn close(&mut self) -> io::Result<B3Digest> {
        if self.task_and_writer.is_none() {
            // if we're already closed, return the b3 digest, which must exist.
            // If it doesn't, we already closed and failed once, and didn't handle the error.
            match &self.digest {
                Some(digest) => Ok(digest.clone()),
                None => Err(io::Error::new(io::ErrorKind::BrokenPipe, "already closed")),
            }
        } else {
            let (task, mut writer) = self.task_and_writer.take().unwrap();

            // invoke shutdown, so the inner writer closes its internal tx side of
            // the channel.
            writer.shutdown().await?;

            // block on the RPC call to return.
            // This ensures all chunks are sent out, and have been received by the
            // backend.

            match task.await? {
                Ok(resp) => {
                    // return the digest from the response, and store it in self.digest for subsequent closes.
                    let digest_len = resp.digest.len();
                    let digest: B3Digest = resp.digest.try_into().map_err(|_| {
                        io::Error::new(
                            io::ErrorKind::Other,
                            format!("invalid root digest length {} in response", digest_len),
                        )
                    })?;
                    self.digest = Some(digest.clone());
                    Ok(digest)
                }
                Err(e) => Err(io::Error::new(io::ErrorKind::Other, e.to_string())),
            }
        }
    }
}

impl<W: tokio::io::AsyncWrite + Unpin> tokio::io::AsyncWrite for GRPCBlobWriter<W> {
    fn poll_write(
        mut self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        buf: &[u8],
    ) -> std::task::Poll<Result<usize, io::Error>> {
        match &mut self.task_and_writer {
            None => Poll::Ready(Err(io::Error::new(
                io::ErrorKind::NotConnected,
                "already closed",
            ))),
            Some((_, ref mut writer)) => {
                let pinned_writer = pin!(writer);
                pinned_writer.poll_write(cx, buf)
            }
        }
    }

    fn poll_flush(
        mut self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Result<(), io::Error>> {
        match &mut self.task_and_writer {
            None => Poll::Ready(Err(io::Error::new(
                io::ErrorKind::NotConnected,
                "already closed",
            ))),
            Some((_, ref mut writer)) => {
                let pinned_writer = pin!(writer);
                pinned_writer.poll_flush(cx)
            }
        }
    }

    fn poll_shutdown(
        self: std::pin::Pin<&mut Self>,
        _cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Result<(), io::Error>> {
        // TODO(raitobezarius): this might not be a graceful shutdown of the
        // channel inside the gRPC connection.
        Poll::Ready(Ok(()))
    }
}

#[cfg(test)]
mod tests {
    use std::time::Duration;

    use tempfile::TempDir;
    use tokio::net::UnixListener;
    use tokio_retry::strategy::ExponentialBackoff;
    use tokio_retry::Retry;
    use tokio_stream::wrappers::UnixListenerStream;

    use crate::blobservice::MemoryBlobService;
    use crate::fixtures;
    use crate::proto::blob_service_client::BlobServiceClient;
    use crate::proto::GRPCBlobServiceWrapper;

    use super::BlobService;
    use super::GRPCBlobService;

    /// This ensures connecting via gRPC works as expected.
    #[tokio::test]
    async fn test_valid_unix_path_ping_pong() {
        let tmpdir = TempDir::new().unwrap();
        let socket_path = tmpdir.path().join("daemon");

        let path_clone = socket_path.clone();

        // Spin up a server
        tokio::spawn(async {
            let uds = UnixListener::bind(path_clone).unwrap();
            let uds_stream = UnixListenerStream::new(uds);

            // spin up a new server
            let mut server = tonic::transport::Server::builder();
            let router =
                server.add_service(crate::proto::blob_service_server::BlobServiceServer::new(
                    GRPCBlobServiceWrapper::new(
                        Box::<MemoryBlobService>::default() as Box<dyn BlobService>
                    ),
                ));
            router.serve_with_incoming(uds_stream).await
        });

        // wait for the socket to be created
        Retry::spawn(
            ExponentialBackoff::from_millis(20).max_delay(Duration::from_secs(10)),
            || async {
                if socket_path.exists() {
                    Ok(())
                } else {
                    Err(())
                }
            },
        )
        .await
        .expect("failed to wait for socket");

        // prepare a client
        let grpc_client = {
            let url = url::Url::parse(&format!(
                "grpc+unix://{}?wait-connect=1",
                socket_path.display()
            ))
            .expect("must parse");
            let client = BlobServiceClient::new(
                crate::tonic::channel_from_url(&url)
                    .await
                    .expect("must succeed"),
            );
            GRPCBlobService::from_client("default".into(), client)
        };

        let has = grpc_client
            .has(&fixtures::BLOB_A_DIGEST)
            .await
            .expect("must not be err");

        assert!(!has);
    }
}
