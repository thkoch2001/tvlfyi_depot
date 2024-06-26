use std::io;

use bytes::Bytes;
use tokio_stream::StreamExt;
use tonic::{async_trait, Code};
use tracing::instrument;

use crate::{proto, B3Digest};

use super::ChunkStore;

/// The maximum total size of a chunk, in bytes
pub const MAX_CHUNK_SIZE: usize = 4 * 1024 * 1024; // 4 MiB

/// The maximum data length in a single BlobChunk gRPC message, in bytes
pub const MAX_CHUNK_PACKET_SIZE: usize = 512 * 1024; // 512KiB

#[derive(Clone)]
pub struct GRPC<T> {
    /// The internal reference to a gRPC client.
    /// Cloning it is cheap, and it internally handles concurrent requests.
    grpc_client: proto::blob_service_client::BlobServiceClient<T>,
}

impl<T> GRPC<T> {
    /// construct a [GRPCBlobService] from a [proto::blob_service_client::BlobServiceClient].
    pub fn from_client(grpc_client: proto::blob_service_client::BlobServiceClient<T>) -> Self {
        Self { grpc_client }
    }
}

#[async_trait]
impl<T> ChunkStore for GRPC<T>
where
    T: tonic::client::GrpcService<tonic::body::BoxBody> + Send + Sync + Clone + 'static,
    T::ResponseBody: tonic::codegen::Body<Data = tonic::codegen::Bytes> + Send + 'static,
    <T::ResponseBody as tonic::codegen::Body>::Error: Into<tonic::codegen::StdError> + Send,
    T::Future: Send,
{
    #[instrument(skip_all, err, fields(chunk.digest=%digest))]
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

    #[instrument(skip_all, err, fields(chunk.digest=%digest))]
    async fn get(&self, digest: &B3Digest) -> io::Result<Option<bytes::Bytes>> {
        match self
            .grpc_client
            .clone()
            .read(proto::ReadBlobRequest {
                digest: digest.clone().into(),
            })
            .await
        {
            Ok(stream) => {
                let mut data_stream = stream.into_inner().map(|elem| elem.map(|c| c.data));
                let mut buf = Vec::new();

                while let Some(d) = data_stream.next().await {
                    let d = d.map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))?;

                    if buf.len() + d.len() > MAX_CHUNK_SIZE {
                        return Err(io::Error::new(
                            io::ErrorKind::Other,
                            "MAX_CHUNK_SIZE reached",
                        ));
                    }

                    buf.extend_from_slice(&d);
                }

                Ok(Some(buf.into()))
            }
            Err(e) if e.code() == Code::NotFound => Ok(None),
            Err(e) => Err(io::Error::new(io::ErrorKind::Other, e)),
        }
    }

    #[instrument(skip_all, err)]
    async fn put(&self, data: bytes::Bytes) -> io::Result<B3Digest> {
        let resp = self
            .grpc_client
            .clone()
            .put(futures::stream::iter(
                data.chunks(MAX_CHUNK_PACKET_SIZE)
                    .map(|d| proto::BlobChunk {
                        data: Bytes::copy_from_slice(d),
                    })
                    .collect::<Vec<_>>(),
            ))
            .await
            .map_err(|e| io::Error::new(io::ErrorKind::Other, e))?
            .into_inner();

        Ok(resp
            .digest
            .try_into()
            .map_err(|_| io::Error::new(io::ErrorKind::Other, "invalid root digest in response"))?)
    }
}
