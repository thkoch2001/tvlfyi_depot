use crate::proto::path_info_service_client::PathInfoServiceClient;

use super::{GRPCPathInfoService, MemoryPathInfoService, PathInfoService, SledPathInfoService};

use std::sync::Arc;
use tvix_castore::{blobservice::BlobService, directoryservice::DirectoryService, Error};
use url::Url;

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
) -> Result<Arc<dyn PathInfoService>, Error> {
    let url =
        Url::parse(uri).map_err(|e| Error::StorageError(format!("unable to parse url: {}", e)))?;

    Ok(if url.scheme() == "memory" {
        // memory doesn't support host or path in the URL.
        if url.has_host() || !url.path().is_empty() {
            return Err(Error::StorageError("invalid url".to_string()));
        }
        Arc::new(MemoryPathInfoService::new(blob_service, directory_service))
    } else if url.scheme() == "sled" {
        // sled doesn't support host, and a path can be provided (otherwise
        // it'll live in memory only).
        if url.has_host() {
            return Err(Error::StorageError("no host allowed".to_string()));
        }

        if url.path() == "/" {
            return Err(Error::StorageError(
                "cowardly refusing to open / with sled".to_string(),
            ));
        }

        // TODO: expose compression and other parameters as URL parameters?

        if url.path().is_empty() {
            return Ok(Arc::new(
                SledPathInfoService::new_temporary(blob_service, directory_service)
                    .map_err(|e| Error::StorageError(e.to_string()))?,
            ));
        }
        return Ok(Arc::new(
            SledPathInfoService::new(url.path().into(), blob_service, directory_service)
                .map_err(|e| Error::StorageError(e.to_string()))?,
        ));
    } else if url.scheme().starts_with("grpc+") {
        // schemes starting with grpc+ go to the GRPCPathInfoService.
        //   That's normally grpc+unix for unix sockets, and grpc+http(s) for the HTTP counterparts.
        // - In the case of unix sockets, there must be a path, but may not be a host.
        // - In the case of non-unix sockets, there must be a host, but no path.
        // Constructing the channel is handled by tvix_castore::channel::from_url.
        let client = PathInfoServiceClient::new(tvix_castore::tonic::channel_from_url(&url).await?);
        Arc::new(GRPCPathInfoService::from_client(client))
    } else {
        Err(Error::StorageError(format!(
            "unknown scheme: {}",
            url.scheme()
        )))?
    })
}

#[cfg(test)]
mod tests {
    use super::from_addr;
    use lazy_static::lazy_static;
    use tempfile::TempDir;
    use test_case::test_case;
    use tvix_castore::utils::{gen_blob_service, gen_directory_service};

    lazy_static! {
        static ref TMPDIR_SLED_1: TempDir = TempDir::new().unwrap();
        static ref TMPDIR_SLED_2: TempDir = TempDir::new().unwrap();
    }

    // the gRPC tests below don't fail, because we connect lazily.

    /// This uses a unsupported scheme.
    #[test_case("http://foo.example/test", false; "unsupported scheme")]
    /// This configures sled in temporary mode.
    #[test_case("sled://", true; "sled valid temporary")]
    /// This configures sled with /, which should fail.
    #[test_case("sled:///", false; "sled invalid root")]
    /// This configures sled with a host, not path, which should fail.
    #[test_case("sled://foo.example", false; "sled invalid host")]
    /// This configures sled with a valid path path, which should succeed.
    #[test_case(&format!("sled://{}", &TMPDIR_SLED_1.path().to_str().unwrap()), true; "sled valid path")]
    /// This configures sled with a host, and a valid path path, which should fail.
    #[test_case(&format!("sled://foo.example{}", &TMPDIR_SLED_2.path().to_str().unwrap()), false; "sled invalid host with valid path")]
    /// This correctly sets the scheme, and doesn't set a path.
    #[test_case("memory://", true; "memory valid")]
    /// This sets a memory url host to `foo`
    #[test_case("memory://foo", false; "memory invalid host")]
    /// This sets a memory url path to "/", which is invalid.
    #[test_case("memory:///", false; "memory invalid root path")]
    /// This sets a memory url path to "/foo", which is invalid.
    #[test_case("memory:///foo", false; "memory invalid root path foo")]
    /// Correct scheme to connect to a unix socket.
    #[test_case("grpc+unix:///path/to/somewhere", true; "grpc valid unix socket")]
    /// Correct scheme for unix socket, but setting a host too, which is invalid.
    #[test_case("grpc+unix://host.example/path/to/somewhere", false; "grpc invalid unix socket and host")]
    /// Correct scheme to connect to localhost, with port 12345
    #[test_case("grpc+http://[::1]:12345", true; "grpc valid IPv6 localhost port 12345")]
    /// Correct scheme to connect to localhost over http, without specifying a port.
    #[test_case("grpc+http://localhost", true; "grpc valid http host without port")]
    /// Correct scheme to connect to localhost over http, without specifying a port.
    #[test_case("grpc+https://localhost", true; "grpc valid https host without port")]
    /// Correct scheme to connect to localhost over http, but with additional path, which is invalid.
    #[test_case("grpc+http://localhost/some-path", false; "grpc valid invalid host and path")]
    #[tokio::test]
    async fn test_from_addr_tokio(uri_str: &str, is_ok: bool) {
        assert_eq!(
            from_addr(uri_str, gen_blob_service(), gen_directory_service())
                .await
                .is_ok(),
            is_ok
        )
    }
}
