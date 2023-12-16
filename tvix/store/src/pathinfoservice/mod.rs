mod from_addr;
mod grpc;
mod memory;
mod nix_http;
mod sled;

use futures::Stream;
use std::pin::Pin;
use tonic::async_trait;
use tvix_castore::proto as castorepb;
use tvix_castore::Error;

use crate::proto::PathInfo;

pub use self::from_addr::from_addr;
pub use self::grpc::GRPCPathInfoService;
pub use self::memory::MemoryPathInfoService;
pub use self::nix_http::NixHTTPPathInfoService;
pub use self::sled::SledPathInfoService;

#[cfg(any(feature = "fuse", feature = "virtiofs"))]
use futures::StreamExt;
#[cfg(any(feature = "fuse", feature = "virtiofs"))]
use std::ops::Deref;
#[cfg(any(feature = "fuse", feature = "virtiofs"))]
use tvix_castore::fs::RootNodes;

/// The base trait all PathInfo services need to implement.
#[async_trait]
pub trait PathInfoService: Send + Sync {
    /// Retrieve a PathInfo message by the output digest.
    async fn get(&self, digest: [u8; 20]) -> Result<Option<PathInfo>, Error>;

    /// Store a PathInfo message. Implementations MUST call validate and reject
    /// invalid messages.
    async fn put(&self, path_info: PathInfo) -> Result<PathInfo, Error>;

    /// Return the nar size and nar sha256 digest for a given root node.
    /// This can be used to calculate NAR-based output paths,
    /// and implementations are encouraged to cache it.
    async fn calculate_nar(
        &self,
        root_node: &castorepb::node::Node,
    ) -> Result<(u64, [u8; 32]), Error>;

    /// Iterate over all PathInfo objects in the store.
    /// Implementations can decide to disallow listing.
    ///
    /// This returns a pinned, boxed stream. The pinning allows for it to be polled easily,
    /// and the box allows different underlying stream implementations to be returned since
    /// Rust doesn't support this as a generic in traits yet. This is the same thing that
    /// [async_trait] generates, but for streams instead of futures.
    fn list(&self) -> Pin<Box<dyn Stream<Item = Result<PathInfo, Error>> + Send>>;
}

/// Implements root node lookup for any [PathInfoService]. This represents a flat
/// directory structure like /nix/store where each entry in the root filesystem
/// directory corresponds to a CA node.
#[cfg(any(feature = "fuse", feature = "virtiofs"))]
#[async_trait]
impl<T> RootNodes for T
where
    T: Deref<Target = dyn PathInfoService> + Send + Sync,
{
    async fn get_by_basename(&self, name: &[u8]) -> Result<Option<castorepb::node::Node>, Error> {
        let Ok(store_path) = nix_compat::store_path::StorePath::from_bytes(name) else {
            return Ok(None);
        };

        Ok(self
            .deref()
            .get(*store_path.digest())
            .await?
            .map(|path_info| {
                path_info
                    .node
                    .expect("missing root node")
                    .node
                    .expect("empty node")
            }))
    }

    fn list(&self) -> Pin<Box<dyn Stream<Item = Result<castorepb::node::Node, Error>> + Send>> {
        Box::pin(self.deref().list().map(|result| {
            result.map(|path_info| {
                path_info
                    .node
                    .expect("missing root node")
                    .node
                    .expect("empty node")
            })
        }))
    }
}
