use crate::proto::blob_service_client::BlobServiceClient;
use std::sync::Arc;
use tokio::net::UnixStream;
use tonic::transport::Endpoint;
use url::Url;

use super::{BlobService, GRPCBlobService, MemoryBlobService, SledBlobService};

/// Constructs a new instance of a [BlobService] from an URI.
///
/// The following URIs are supported:
/// - `memory:`
///   Uses a in-memory implementation.
/// - `sled:`
///   Uses a in-memory sled implementation.
/// - `sled:///absolute/path/to/somewhere`
///   Uses sled, using a path on the disk for persistency. Can be only opened
///   from one process at the same time.
/// - `grpc+unix:///absolute/path/to/somewhere`
///   Connects to a local tvix-store gRPC service via Unix socket.
/// - `grpc+http://host:port`, `grpc+https://host:port`
///    Connects to a (remote) tvix-store gRPC service.
pub async fn from_addr(uri: &str) -> Result<Arc<dyn BlobService>, crate::Error> {
    let url = Url::parse(uri).map_err(|e| crate::Error::StorageError(e.to_string()))?;

    Ok(if url.scheme() == "memory" {
        Arc::new(MemoryBlobService::default())
    } else if url.scheme() == "sled" {
        // extract path.
        if url.path().is_empty() {
            Arc::new(
                SledBlobService::new_temporary()
                    .map_err(|e| crate::Error::StorageError(e.to_string()))?,
            )
        } else {
            Arc::new(
                SledBlobService::new(url.path().into())
                    .map_err(|e| crate::Error::StorageError(e.to_string()))?,
            )
        }
    } else if let Some(uri) = uri.strip_prefix("grpc+") {
        // check if the remainder starts with unix://
        if uri.starts_with("unix://") {
            let url = Url::parse(uri).map_err(|e| crate::Error::StorageError(e.to_string()))?;
            let socket_path = url.path().to_string();

            let channel = Endpoint::try_from("http://[::]:50051") // doesn't matter
                .unwrap()
                .connect_with_connector_lazy(tower::service_fn(move |_: tonic::transport::Uri| {
                    UnixStream::connect(socket_path.clone())
                }));
            let grpc_client = BlobServiceClient::new(channel);
            Arc::new(GRPCBlobService::new(
                grpc_client,
                tokio::runtime::Handle::current(),
            ))
        } else {
            Arc::new(GRPCBlobService::from_client(
                BlobServiceClient::connect(uri.to_string())
                    .await
                    .map_err(|e| crate::Error::StorageError(e.to_string()))?,
            ))
        }
    } else {
        Err(crate::Error::StorageError(format!(
            "invalid scheme: {}",
            url.scheme()
        )))?
    })
}
