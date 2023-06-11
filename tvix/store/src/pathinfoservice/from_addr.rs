use std::sync::Arc;

use tokio::net::UnixStream;
use tonic::transport::Endpoint;
use url::Url;

use crate::{
    blobservice::BlobService, directoryservice::DirectoryService,
    proto::path_info_service_client::PathInfoServiceClient,
};

use super::{GRPCPathInfoService, MemoryPathInfoService, PathInfoService, SledPathInfoService};

/// Constructs a new instance of a [PathInfoService] from an URI.
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
///
/// As the [PathInfoService] needs to talk to [BlobService] and [DirectoryService],
/// these also need to be passed in.
pub async fn from_addr(
    uri: &str,
    blob_service: Arc<dyn BlobService>,
    directory_service: Arc<dyn DirectoryService>,
) -> Result<Arc<dyn PathInfoService>, crate::Error> {
    let url = Url::parse(uri).map_err(|e| crate::Error::StorageError(e.to_string()))?;

    Ok(if url.scheme() == "memory" {
        Arc::new(MemoryPathInfoService::new(blob_service, directory_service))
    } else if url.scheme() == "sled" {
        // extract path.
        if url.path().is_empty() {
            Arc::new(
                SledPathInfoService::new_temporary(blob_service, directory_service)
                    .map_err(|e| crate::Error::StorageError(e.to_string()))?,
            )
        } else {
            Arc::new(
                SledPathInfoService::new(url.path().into(), blob_service, directory_service)
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
            let grpc_client = PathInfoServiceClient::new(channel);
            Arc::new(GRPCPathInfoService::from_client(grpc_client))
        } else {
            Arc::new(GRPCPathInfoService::from_client(
                PathInfoServiceClient::connect(uri.to_string())
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
