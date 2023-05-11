//! This module provides an implementation of EvalIO.
//!
//! It can be used by the tvix evalutator to talk to a tvix store.

use nix_compat::{
    derivation::{Derivation, Output},
    nixhash::{HashAlgo, NixHash, NixHashWithMode},
    store_path::StorePath,
};
use smol_str::SmolStr;
use std::{io, path::Path, path::PathBuf};
use tracing::warn;
use tvix_eval::{EvalIO, FileType, StdIO};

use crate::{
    blobservice::BlobService,
    directoryservice::{self, DirectoryService},
    import,
    nar::NARCalculationService,
    pathinfoservice::PathInfoService,
    proto::NamedNode,
};

/// Implements [EvalIO], asking given [PathInfoService], [DirectoryService]
/// and [BlobService].
///
/// In case the given path does not exist in these stores, we ask StdIO.
/// This is to both cover cases of syntactically valid store paths, that exist
/// on the filesystem (still managed by Nix), as well as being able to read
/// files outside store paths.
pub struct TvixStoreIO<
    BS: BlobService,
    DS: DirectoryService,
    PS: PathInfoService,
    NCS: NARCalculationService,
> {
    blob_service: BS,
    directory_service: DS,
    path_info_service: PS,
    nar_calculation_service: NCS,
    std_io: StdIO,
}

impl<BS: BlobService, DS: DirectoryService, PS: PathInfoService, NCS: NARCalculationService>
    TvixStoreIO<BS, DS, PS, NCS>
{
    pub fn new(
        blob_service: BS,
        directory_service: DS,
        path_info_service: PS,
        nar_calculation_service: NCS,
    ) -> Self {
        Self {
            blob_service,
            directory_service,
            path_info_service,
            nar_calculation_service,
            std_io: StdIO {},
        }
    }

    /// for a given [StorePath] and additional [Path] inside the store path,
    /// look up the [PathInfo], and if it exists, traverse the directory structure to
    /// return the [crate::proto::node::Node] specified by `sub_path`.
    fn store_path_to_root_node(
        &mut self,
        store_path: &StorePath,
        sub_path: &Path,
    ) -> Result<Option<crate::proto::node::Node>, crate::Error> {
        let path_info = {
            match self.path_info_service.get(store_path.digest)? {
                // If there's no PathInfo found, early exit
                None => return Ok(None),
                Some(path_info) => path_info,
            }
        };

        let root_node = {
            match path_info.node {
                None => {
                    warn!(
                        "returned PathInfo {:?} node is None, this shouldn't happen.",
                        &path_info
                    );
                    return Ok(None);
                }
                Some(root_node) => match root_node.node {
                    None => {
                        warn!("node for {:?} is None, this shouldn't happen.", &root_node);
                        return Ok(None);
                    }
                    Some(root_node) => root_node,
                },
            }
        };

        directoryservice::traverse_to(&mut self.directory_service, root_node, sub_path)
    }

    /// Imports a given path on the filesystem into the store, and returns the
    /// [crate::proto::PathInfo] describing the path, that was sent to
    /// [PathInfoService].
    /// While not part of the [EvalIO], it's still useful for clients who
    /// care about the [PathInfo].
    pub fn import_path_with_pathinfo(
        &mut self,
        path: &std::path::Path,
    ) -> Result<crate::proto::PathInfo, io::Error> {
        // Call [import::ingest_path], which will walk over the given path and return a root_node.
        let root_node =
            import::ingest_path(&mut self.blob_service, &mut self.directory_service, path)
                .expect("error during import_path");

        // Render the NAR
        let (nar_size, nar_sha256) = self
            .nar_calculation_service
            .calculate_nar(&root_node)
            .expect("error during nar calculation"); // TODO: handle error

        // Calculate the NAR-based StorePath
        let output_path = calculate_nar_based_store_path(
            &nar_sha256,
            path.file_name()
                .expect("path must not be ..")
                .to_str()
                .expect("path must be valid unicode"),
        );

        // assemble a new root_node with a name that is derived from the nar hash.
        let renamed_root_node = {
            let name = output_path.to_string();

            match root_node {
                crate::proto::node::Node::Directory(n) => {
                    crate::proto::node::Node::Directory(crate::proto::DirectoryNode { name, ..n })
                }
                crate::proto::node::Node::File(n) => {
                    crate::proto::node::Node::File(crate::proto::FileNode { name, ..n })
                }
                crate::proto::node::Node::Symlink(n) => {
                    crate::proto::node::Node::Symlink(crate::proto::SymlinkNode { name, ..n })
                }
            }
        };

        // assemble the [crate::proto::PathInfo] object.
        let path_info = crate::proto::PathInfo {
            node: Some(crate::proto::Node {
                node: Some(renamed_root_node),
            }),
            // There's no reference scanning on path contents ingested like this.
            references: vec![],
            narinfo: Some(crate::proto::NarInfo {
                nar_size,
                nar_sha256: nar_sha256.to_vec(),
                signatures: vec![],
                reference_names: vec![],
                // TODO: narinfo for talosctl.src contains `CA: fixed:r:sha256:1x13j5hy75221bf6kz7cpgld9vgic6bqx07w5xjs4pxnksj6lxb6`
                // do we need this anywhere?
            }),
        };

        // put into [PathInfoService], and return the PathInfo that we get back
        // from there (it might contain additional signatures).
        let path_info = self.path_info_service.put(path_info)?;

        Ok(path_info)
    }
}

/// For given NAR sha256 digest and name, return the new [StorePath] this would have.
fn calculate_nar_based_store_path(nar_sha256_digest: &[u8; 32], name: &str) -> StorePath {
    let nar_hash_with_mode =
        NixHashWithMode::Recursive(NixHash::new(HashAlgo::Sha256, nar_sha256_digest.to_vec()));

    let output_path = {
        let mut drv = Derivation::default();
        drv.outputs.insert(
            "out".to_string(),
            Output {
                path: "".to_string(),
                hash_with_mode: Some(nar_hash_with_mode),
            },
        );
        drv.calculate_output_paths(
            name,
            // Note the derivation_or_fod_hash argument is *unused* for FODs, so it doesn't matter what we pass here.
            &NixHash::new(HashAlgo::Sha256, vec![]),
        )
        .expect("error during output path calculation");

        drv.outputs.get("out").unwrap().path.to_string()
    };

    // convert to StorePath, until cl/8412
    StorePath::from_absolute_path(&output_path).unwrap()
}

impl<
        BS: BlobService + Clone,
        DS: DirectoryService + Clone,
        PS: PathInfoService,
        NCS: NARCalculationService,
    > EvalIO for TvixStoreIO<BS, DS, PS, NCS>
{
    fn path_exists(&mut self, path: &Path) -> Result<bool, io::Error> {
        if let Ok((store_path, sub_path)) =
            StorePath::from_absolute_path_full(&path.to_string_lossy())
        {
            if self
                .store_path_to_root_node(&store_path, &sub_path)?
                .is_some()
            {
                Ok(true)
            } else {
                // As tvix-store doesn't manage /nix/store on the filesystem,
                // we still need to also ask self.std_io here.
                self.std_io.path_exists(path)
            }
        } else {
            // The store path is no store path, so do regular StdIO.
            self.std_io.path_exists(path)
        }
    }

    fn read_to_string(&mut self, path: &Path) -> Result<String, io::Error> {
        if let Ok((store_path, sub_path)) =
            StorePath::from_absolute_path_full(&path.to_string_lossy())
        {
            if let Some(node) = self.store_path_to_root_node(&store_path, &sub_path)? {
                // depending on the node type, treat read_to_string differently
                match node {
                    crate::proto::node::Node::Directory(_) => {
                        // This would normally be a io::ErrorKind::IsADirectory (still unstable)
                        Err(io::Error::new(
                            io::ErrorKind::Unsupported,
                            "tried to read directory at {path} to string",
                        ))
                    }
                    crate::proto::node::Node::File(file_node) => {
                        let digest: [u8; 32] =
                            file_node.digest.clone().try_into().map_err(|_e| {
                                io::Error::new(
                                    io::ErrorKind::InvalidData,
                                    format!("invalid digest length in file node: {:?}", file_node),
                                )
                            })?;

                        let digest_b64 = data_encoding::BASE64.encode(&digest);

                        let reader = {
                            let resp = self.blob_service.open_read(&digest)?;
                            match resp {
                                Some(blob_reader) => blob_reader,
                                None => Err(io::Error::new(
                                    io::ErrorKind::NotFound,
                                    format!("blob {} not found", &digest_b64),
                                ))?,
                            }
                        };

                        io::read_to_string(reader)
                    }
                    crate::proto::node::Node::Symlink(_symlink_node) => Err(io::Error::new(
                        io::ErrorKind::Unsupported,
                        "read_to_string for symlinks is unsupported",
                    ))?,
                }
            } else {
                // As tvix-store doesn't manage /nix/store on the filesystem,
                // we still need to also ask self.std_io here.
                self.std_io.read_to_string(path)
            }
        } else {
            // The store path is no store path, so do regular StdIO.
            self.std_io.read_to_string(path)
        }
    }

    fn read_dir(&mut self, path: &Path) -> Result<Vec<(SmolStr, FileType)>, io::Error> {
        if let Ok((store_path, sub_path)) =
            StorePath::from_absolute_path_full(&path.to_string_lossy())
        {
            if let Some(node) = self.store_path_to_root_node(&store_path, &sub_path)? {
                match node {
                    crate::proto::node::Node::Directory(directory_node) => {
                        // fetch the Directory itself.
                        let digest: [u8; 32] =
                            directory_node.digest.clone().try_into().map_err(|_e| {
                                io::Error::new(
                                    io::ErrorKind::InvalidData,
                                    format!(
                                        "invalid digest length in directory node: {:?}",
                                        directory_node
                                    ),
                                )
                            })?;
                        let digest_b64 = data_encoding::BASE64.encode(&digest);

                        if let Some(directory) = self.directory_service.get(&digest)? {
                            let mut children: Vec<(SmolStr, FileType)> = Vec::new();
                            for node in directory.nodes() {
                                children.push(match node {
                                    crate::proto::node::Node::Directory(e) => {
                                        (e.name.into(), FileType::Directory)
                                    }
                                    crate::proto::node::Node::File(e) => {
                                        (e.name.into(), FileType::Regular)
                                    }
                                    crate::proto::node::Node::Symlink(e) => {
                                        (e.name.into(), FileType::Symlink)
                                    }
                                })
                            }
                            Ok(children)
                        } else {
                            // If we didn't get the directory node that's linked, that's a store inconsistency, bail out!
                            warn!(
                                directory_node = &digest_b64,
                                path = ?path,
                                "directory node does not exist",
                            );
                            Err(io::Error::new(
                                io::ErrorKind::NotFound,
                                format!("directory {digest_b64} does not exist"),
                            ))?
                        }
                    }
                    crate::proto::node::Node::File(_file_node) => {
                        // This would normally be a io::ErrorKind::NotADirectory (still unstable)
                        Err(io::Error::new(
                            io::ErrorKind::Unsupported,
                            "tried to readdir path {:?}, which is a file",
                        ))?
                    }
                    crate::proto::node::Node::Symlink(_symlink_node) => Err(io::Error::new(
                        io::ErrorKind::Unsupported,
                        "read_dir for symlinks is unsupported",
                    ))?,
                }
            } else {
                self.std_io.read_dir(path)
            }
        } else {
            self.std_io.read_dir(path)
        }
    }

    fn import_path(&mut self, path: &std::path::Path) -> Result<PathBuf, std::io::Error> {
        let path_info = self.import_path_with_pathinfo(path)?;

        // from the [PathInfo], extract the store path (as string).
        let mut path = PathBuf::from(nix_compat::store_path::STORE_DIR_WITH_SLASH);
        path.push(path_info.node.unwrap().node.unwrap().get_name());

        // and return it
        Ok(path)
    }

    fn store_dir(&self) -> Option<String> {
        Some("/nix/store".to_string())
    }
}
