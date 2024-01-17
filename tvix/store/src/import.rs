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

pub fn log_node(node: &Node, path: &Path, name: &str) {
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

/// Transforms a given path and returns its corresponding [`Node`] by interacting with
/// [`BlobService`] and [`DirectoryService`] for ingestion.
/// Returns the path information object for a NAR-style (recursive) fixed-output path.
#[inline]
pub fn derive_recursive_fod_path_info(
    nar_size: u64,
    nar_sha256: [u8; 32],
    root_node: Node,
) -> PathInfo {
    // assemble the [crate::proto::PathInfo] object.
    PathInfo {
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
    }
}

/// Ingest the given path [`path`] and register the resulting output path in the
/// [`PathInfoService`] as a recursive fixed output NAR.
#[instrument(skip_all, fields(path=?path), err)]
pub async fn import_path_as_recursive_fod<BS, DS, PS, P>(
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
    let root_node =
        tvix_castore::import::ingest_path(blob_service, directory_service, &path).await?;

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
    log_node(&root_node, path.as_ref(), name);

    let path_info = derive_recursive_fod_path_info(nar_size, nar_sha256, root_node);

    // put into [PathInfoService], and return the PathInfo that we get back
    // from there (it might contain additional signatures).
    let _path_info = path_info_service.as_ref().put(path_info).await?;

    Ok(output_path.to_owned())
}
