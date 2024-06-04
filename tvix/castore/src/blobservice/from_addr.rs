use std::sync::Arc;

use url::Url;

use crate::{proto::blob_service_client::BlobServiceClient, Error};
use crate::blobservice::CombinedBlobService;

use super::{BlobService, GRPCBlobService, MemoryBlobService, ObjectStoreBlobService};

/// Constructs a new instance of a [BlobService] from an URI.
///
/// The following schemes are supported by the following services:
/// - `memory://` ([MemoryBlobService])
/// - `grpc+*://` ([GRPCBlobService])
/// - `objectstore+*://` ([ObjectStoreBlobService])
///
/// See their `from_url` methods for more details about their syntax.
pub async fn from_addr(uri: &str) -> Result<Arc<dyn BlobService>, crate::Error> {
    let url = Url::parse(uri)
        .map_err(|e| crate::Error::StorageError(format!("unable to parse url: {}", e)))?;

    let mut blob_service: Arc<dyn BlobService> = match url.scheme() {
        "memory" => {
            // memory doesn't support host or path in the URL.
            if url.has_host() || !url.path().is_empty() {
                return Err(Error::StorageError("invalid url".to_string()));
            }
            Arc::<MemoryBlobService>::default()
        }
        scheme if scheme.starts_with("grpc+") => {
            // schemes starting with grpc+ go to the GRPCPathInfoService.
            //   That's normally grpc+unix for unix sockets, and grpc+http(s) for the HTTP counterparts.
            // - In the case of unix sockets, there must be a path, but may not be a host.
            // - In the case of non-unix sockets, there must be a host, but no path.
            // Constructing the channel is handled by tvix_castore::channel::from_url.
            let client = BlobServiceClient::new(crate::tonic::channel_from_url(&url).await?);
            Arc::new(GRPCBlobService::from_client(client))
        }
        scheme if scheme.starts_with("objectstore+") => {
            // We need to convert the URL to string, strip the prefix there, and then
            // parse it back as url, as Url::set_scheme() rejects some of the transitions we want to do.
            let trimmed_url = {
                let s = url.to_string();
                Url::parse(s.strip_prefix("objectstore+").unwrap()).unwrap()
            };
            Arc::new(
                ObjectStoreBlobService::parse_url(&trimmed_url)
                    .map_err(|e| Error::StorageError(e.to_string()))?,
            )
        }
        scheme => {
            return Err(crate::Error::StorageError(format!(
                "unknown scheme: {}",
                scheme
            )))
        }
    };
    let query = url.query_pairs().map(|(a, b)| (a.to_string(), b.to_string())).collect::<std::collections::HashMap<String, String>>();
    if let Some(cache_setting) = query.get("cache") {
        let cache = Box::pin(from_addr(cache_setting)).await?;
        blob_service = Arc::new(CombinedBlobService::new(cache, blob_service));
    }

    Ok(blob_service)
}

#[cfg(test)]
mod tests {
    use super::from_addr;
    use rstest::rstest;

    #[rstest]
    /// This uses an unsupported scheme.
    #[case::unsupported_scheme("http://foo.example/test", false)]
    /// This correctly sets the scheme, and doesn't set a path.
    #[case::memory_valid("memory://", true)]
    /// This sets a memory url host to `foo`
    #[case::memory_invalid_host("memory://foo", false)]
    /// This sets a memory url path to "/", which is invalid.
    #[case::memory_invalid_root_path("memory:///", false)]
    /// This sets a memory url path to "/foo", which is invalid.
    #[case::memory_invalid_root_path_foo("memory:///foo", false)]
    /// Correct scheme to connect to a unix socket.
    #[case::grpc_valid_unix_socket("grpc+unix:///path/to/somewhere", true)]
    /// Correct scheme for unix socket, but setting a host too, which is invalid.
    #[case::grpc_invalid_unix_socket_and_host("grpc+unix://host.example/path/to/somewhere", false)]
    /// Correct scheme to connect to localhost, with port 12345
    #[case::grpc_valid_ipv6_localhost_port_12345("grpc+http://[::1]:12345", true)]
    /// Correct scheme to connect to localhost over http, without specifying a port.
    #[case::grpc_valid_http_host_without_port("grpc+http://localhost", true)]
    /// Correct scheme to connect to localhost over http, without specifying a port.
    #[case::grpc_valid_https_host_without_port("grpc+https://localhost", true)]
    /// Correct scheme to connect to localhost over http, but with additional path, which is invalid.
    #[case::grpc_invalid_has_path("grpc+http://localhost/some-path", false)]
    /// An example for object store (InMemory)
    #[case::objectstore_valid_memory("objectstore+memory:///", true)]
    /// An example for object store (LocalFileSystem)
    #[case::objectstore_valid_file("objectstore+file:///foo/bar", true)]
    // An example for object store (HTTP / WebDAV)
    #[case::objectstore_valid_http_url("objectstore+https://localhost:8080/some-path", true)]
    /// An example for object store (S3)
    #[cfg_attr(
        feature = "cloud",
        case::objectstore_valid_s3_url("objectstore+s3://bucket/path", true)
    )]
    /// An example for object store (GCS)
    #[cfg_attr(
        feature = "cloud",
        case::objectstore_valid_gcs_url("objectstore+gs://bucket/path", true)
    )]
    #[tokio::test]
    async fn test_from_addr_tokio(#[case] uri_str: &str, #[case] exp_succeed: bool) {
        if exp_succeed {
            from_addr(uri_str).await.expect("should succeed");
        } else {
            assert!(from_addr(uri_str).await.is_err(), "should fail");
        }
    }
}
