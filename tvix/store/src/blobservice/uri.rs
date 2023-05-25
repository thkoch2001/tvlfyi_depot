use crate::proto::blob_service_client::BlobServiceClient;
use tokio::net::UnixStream;
use tonic::transport::{Endpoint, Uri};
use url::Url;

use super::{BlobService, GRPCBlobService, MemoryBlobService, SledBlobService};

/// Constructs a new instance of a [BlobService] from an URI.
///
/// The following URIs are supported:
/// - `memory:`
///   Uses a in-memory implementation.
/// - `sled:`
///   Uses a in-memory sled implementation.
/// - `sled:///path/to/somewhere`
///   Uses sled, using a path on the disk for persistency. Can be only opened
///   from one process at the same time.
/// - `grpc+unix:///path/to/somewhere`
///   Connects to a local tvix-store gRPC service via Unix socket.
/// - `grpc+http://host:port`, `grpc+https://host:port`
///    Connects to a (remote) tvix-store gRPC service.
pub async fn from_uri(uri: &str) -> Result<Box<dyn BlobService>, crate::Error> {
    let url = Url::parse(&uri).map_err(|e| crate::Error::StorageError(e.to_string()))?;

    Ok(if url.scheme() == "memory" {
        Box::new(MemoryBlobService::default())
    } else if url.scheme() == "sled" {
        // extract path.
        if url.path().is_empty() {
            Box::new(
                SledBlobService::new_temporary()
                    .map_err(|e| crate::Error::StorageError(e.to_string()))?,
            )
        } else {
            Box::new(
                SledBlobService::new(url.path().into())
                    .map_err(|e| crate::Error::StorageError(e.to_string()))?,
            )
        }
    } else if let Some(uri) = uri.strip_prefix("grpc+") {
        // check if the remainder starts with unix://
        if uri.starts_with("unix://") {
            let url = Url::parse(&uri).map_err(|e| crate::Error::StorageError(e.to_string()))?;
            let socket_path = url.path().to_string();

            let channel = Endpoint::try_from("http://[::]:50051") // doesn't matter
                .unwrap()
                .connect_with_connector_lazy(tower::service_fn(move |_: Uri| {
                    UnixStream::connect(socket_path.clone())
                }));
            let grpc_client = BlobServiceClient::new(channel);
            Box::new(GRPCBlobService::new(
                grpc_client,
                tokio::runtime::Handle::current(),
            ))
        } else {
            Box::new(GRPCBlobService::from_client(
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
