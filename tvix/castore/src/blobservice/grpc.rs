use super::{BlobService, ChunkMeta};
use crate::{proto, B3Digest};
use std::io;
use tonic::{async_trait, Code};
use tracing::instrument;

/// Connects to a (remote) tvix-store BlobService over gRPC.
#[derive(Clone)]
pub struct GRPCBlobService<T> {
    /// The internal reference to a gRPC client.
    /// Cloning it is cheap, and it internally handles concurrent requests.
    grpc_client: proto::blob_service_client::BlobServiceClient<T>,
}

impl<T> GRPCBlobService<T> {
    /// construct a [GRPCBlobService] from a [proto::blob_service_client::BlobServiceClient].
    /// panics if called outside the context of a tokio runtime.
    pub fn from_client(grpc_client: proto::blob_service_client::BlobServiceClient<T>) -> Self {
        Self { grpc_client }
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
    #[instrument(skip(self, digest), fields(blob.digest=%digest))]
    async fn has(&self, digest: &B3Digest) -> io::Result<bool> {
        let mut grpc_client = self.grpc_client.clone();
        let resp = grpc_client
            .stat(proto::StatBlobRequest {
                digest: digest.clone().into(),
                ..Default::default()
            })
            .await;

        match resp {
            Ok(_blob_meta) => Ok(true),
            Err(e) if e.code() == Code::NotFound => Ok(false),
            Err(e) => Err(io::Error::new(io::ErrorKind::Other, e)),
        }
    }

    async fn put(&self, digest: B3Digest, chunks: &[ChunkMeta]) -> io::Result<()> {
        // Not really clear what to do here, we sort of have to either:
        // - Expose more chunking details over gRPC so the BlobWriter will work
        // - Add another layer on top of chunk/blob, either:
        //    - Remote blob/chunk store which uses gRPC and doesn't expose chunk details
        //    - In-process blob/chunk store backed by the blob & chunk service
        // Sort of in favor of the latter: it lets us simplify a lot of the chunk and blob
        // handling internally, but also keeps the gRPC interface clean.
        todo!("ugh not sure what we are supposed to do here...")
    }

    #[instrument(skip(self, digest), fields(blob.digest=%digest), err)]
    async fn chunks(&self, digest: &B3Digest) -> io::Result<Option<Vec<super::ChunkMeta>>> {
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

                let chunks = resp
                    .chunks
                    .into_iter()
                    .map(|chunk| chunk.try_into())
                    .collect::<Result<_, _>>()
                    .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))?;

                Ok(Some(chunks))
            }
        }
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
            GRPCBlobService::from_client(client)
        };

        let has = grpc_client
            .has(&fixtures::BLOB_A_DIGEST)
            .await
            .expect("must not be err");

        assert!(!has);
    }
}
