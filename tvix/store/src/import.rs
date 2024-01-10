use std::path::Path;

use data_encoding::BASE64;
use tracing::{debug, instrument};
use tvix_castore::{
    blobservice::BlobService, directoryservice::DirectoryService, proto::node::Node,
};

use nix_compat::store_path::{self, StorePath};

use crate::{
    pathinfoservice::PathInfoService,
    proto::{nar_info, NarInfo, PathInfo},
};

fn log_node(node: &Node, path: &Path, name: &str) {
    match node {
        Node::Directory(directory_node) => {
            debug!(
                path = ?path,
                store_name = ?name,
                name = ?directory_node.name,
                digest = BASE64.encode(&directory_node.digest),
                "import successful",
            )
        }
        Node::File(file_node) => {
            debug!(
                path = ?path,
                store_name = ?name,
                name = ?file_node.name,
                digest = BASE64.encode(&file_node.digest),
                "import successful"
            )
        }
        Node::Symlink(symlink_node) => {
            debug!(
                path = ?path,
                store_name = ?name,
                name = ?symlink_node.name,
                target = ?symlink_node.target,
                "import successful"
            )
        }
    }
}

/// Transform a path into its base name and returns an [`std::io::Error`] if it is `..` or if the
/// basename is not valid unicode.
#[inline]
pub fn path_to_name(path: &Path) -> std::io::Result<&str> {
    path.file_name()
        .and_then(|file_name| file_name.to_str())
        .ok_or_else(|| {
            std::io::Error::new(
                std::io::ErrorKind::InvalidInput,
                "path must not be .. and the basename valid unicode",
            )
        })
}

/// Imports a given path on the filesystem into the store, and returns the
/// [`StorePath`] describing the path, that was sent to
/// [`PathInfoService`].
#[instrument(skip_all, fields(path=?path), err)]
pub async fn import_path<BS, DS, PS, P>(
    path: P,
    name: &str,
    blob_service: BS,
    directory_service: DS,
    path_info_service: PS,
) -> Result<StorePath, std::io::Error>
where
    P: AsRef<Path> + std::fmt::Debug,
    BS: AsRef<dyn BlobService> + Clone,
    DS: AsRef<dyn DirectoryService>,
    PS: AsRef<dyn PathInfoService>,
{
    let root_node = path_to_root_node(path.as_ref(), blob_service, directory_service).await?;
    import_root_node(path_info_service, path.as_ref(), name, root_node).await
}

/// Transforms a given path and returns its corresponding [`Node`] by interacting with
/// [`BlobService`] and [`DirectoryService`] for ingestion.
pub async fn path_to_root_node<P, BS, DS>(
    path: P,
    blob_service: BS,
    directory_service: DS,
) -> std::io::Result<Node>
where
    P: AsRef<Path> + std::fmt::Debug,
    BS: AsRef<dyn BlobService> + Clone,
    DS: AsRef<dyn DirectoryService>,
{
    tvix_castore::import::ingest_path(blob_service, directory_service, &path)
        .await
        .map_err(|err| err.into())
}

/// Imports a root [`Node`] which was obtained at a given path using an [`PathInfoService`] and
/// returns a [`StorePath`] corresponding to that import.
pub async fn import_root_node<PS>(
    path_info_service: PS,
    path: &Path,
    name: &str,
    root_node: Node,
) -> std::io::Result<StorePath>
where
    PS: AsRef<dyn PathInfoService>,
{
    // Ask the PathInfoService for the NAR size and sha256
    let (nar_size, nar_sha256) = path_info_service.as_ref().calculate_nar(&root_node).await?;

    // Calculate the output path. This might still fail, as some names are illegal.
    let output_path = store_path::build_nar_based_store_path(&nar_sha256, name).map_err(|_| {
        std::io::Error::new(
            std::io::ErrorKind::InvalidData,
            format!("invalid name: {}", name),
        )
    })?;

    // assemble a new root_node with a name that is derived from the nar hash.
    let root_node = root_node.rename(output_path.to_string().into_bytes().into());
    log_node(&root_node, path, name);

    // assemble the [crate::proto::PathInfo] object.
    let path_info = PathInfo {
        node: Some(tvix_castore::proto::Node {
            node: Some(root_node),
        }),
        // There's no reference scanning on path contents ingested like this.
        references: vec![],
        narinfo: Some(NarInfo {
            nar_size,
            nar_sha256: nar_sha256.to_vec().into(),
            signatures: vec![],
            reference_names: vec![],
            deriver: None,
            ca: Some(nar_info::Ca {
                r#type: nar_info::ca::Hash::NarSha256.into(),
                digest: nar_sha256.to_vec().into(),
            }),
        }),
    };

    // put into [PathInfoService], and return the PathInfo that we get back
    // from there (it might contain additional signatures).
    let _path_info = path_info_service.as_ref().put(path_info).await?;

    Ok(output_path.to_owned())
}
