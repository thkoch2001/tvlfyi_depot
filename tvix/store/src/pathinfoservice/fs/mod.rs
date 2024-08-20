use futures::stream::BoxStream;
use futures::StreamExt;
use nix_compat::store_path::StorePathRef;
use tonic::async_trait;
use tvix_castore::fs::{RootNodes, TvixStoreFs};
use tvix_castore::{blobservice::BlobService, directoryservice::DirectoryService};
use tvix_castore::{Error, Node, PathComponent};

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
    show_xattr: bool,
) -> TvixStoreFs<BS, DS, RootNodesWrapper<PS>>
where
    BS: AsRef<dyn BlobService> + Send + Clone + 'static,
    DS: AsRef<dyn DirectoryService> + Send + Clone + 'static,
    PS: AsRef<dyn PathInfoService> + Send + Sync + Clone + 'static,
{
    TvixStoreFs::new(
        blob_service,
        directory_service,
        RootNodesWrapper(path_info_service),
        list_root,
        show_xattr,
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
    T: AsRef<dyn PathInfoService> + Send + Sync,
{
    async fn get_by_basename(&self, name: &PathComponent) -> Result<Option<Node>, Error> {
        let Ok(store_path) = StorePathRef::from_bytes(name.as_ref()) else {
            return Ok(None);
        };

        Ok(self
            .0
            .as_ref()
            .get(*store_path.digest())
            .await?
            .map(|path_info| {
                let node = path_info
                    .node
                    .as_ref()
                    .expect("missing root node")
                    .to_owned();

                match node.into_name_and_node() {
                    Ok((_name, node)) => Ok(node),
                    Err(e) => Err(Error::StorageError(e.to_string())),
                }
            })
            .transpose()?)
    }

    fn list(&self) -> BoxStream<Result<(PathComponent, Node), Error>> {
        Box::pin(self.0.as_ref().list().map(|result| {
            result.and_then(|path_info| {
                let node = path_info
                    .node
                    .as_ref()
                    .expect("missing root node")
                    .to_owned();

                node.into_name_and_node()
                    .map_err(|e| Error::StorageError(e.to_string()))
            })
        }))
    }
}
