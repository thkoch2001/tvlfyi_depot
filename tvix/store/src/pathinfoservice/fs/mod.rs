use futures::stream::BoxStream;
use futures::StreamExt;
use tonic::async_trait;
use tvix_castore::fs::{RootNodes, TvixStoreFs};
use tvix_castore::proto as castorepb;
use tvix_castore::Error;
use tvix_castore::{blobservice::BlobService, directoryservice::DirectoryService};

use super::PathInfoService;

/// Helper to construct a [TvixStoreFs] from a [BlobService], [DirectoryService]
/// and [PathInfoService].
/// This avoids users to have to interact with the wrapper struct directly, as
/// it leaks into the type signature of TvixStoreFS.
pub fn make_fs<BS, DS, PS>(
    blob_service: BS,
    directory_service: DS,
    path_info_service: PS,
    list_root: bool,
) -> TvixStoreFs<BS, DS, RootNodesWrapper<PS>>
where
    BS: BlobService,
    DS: DirectoryService,
    PS: PathInfoService,
{
    TvixStoreFs::new(
        blob_service,
        directory_service,
        RootNodesWrapper(path_info_service),
        list_root,
    )
}

/// Wrapper to satisfy Rust's orphan rules for trait implementations, as
/// RootNodes is coming from the [tvix-castore] crate.
#[doc(hidden)]
#[derive(Clone, Debug)]
pub struct RootNodesWrapper<T>(pub(crate) T);

/// Implements root node lookup for any [PathInfoService]. This represents a flat
/// directory structure like /nix/store where each entry in the root filesystem
/// directory corresponds to a CA node.
#[cfg(any(feature = "fuse", feature = "virtiofs"))]
#[async_trait]
impl<T> RootNodes for RootNodesWrapper<T>
where
    T: PathInfoService,
{
    async fn get_by_basename(&self, name: &[u8]) -> Result<Option<castorepb::node::Node>, Error> {
        let Ok(store_path) = nix_compat::store_path::StorePath::from_bytes(name) else {
            return Ok(None);
        };

        Ok(self.0.get(*store_path.digest()).await?.map(|path_info| {
            path_info
                .node
                .expect("missing root node")
                .node
                .expect("empty node")
        }))
    }

    fn list(&self) -> BoxStream<Result<castorepb::node::Node, Error>> {
        Box::pin(self.0.list().map(|result| {
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
