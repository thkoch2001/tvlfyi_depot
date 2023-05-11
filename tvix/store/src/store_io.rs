//! This module provides an implementation of EvalIO.
//!
//! It can be used by the tvix evalutator to talk to a tvix store.

use nix_compat::{
    derivation::{Derivation, Output},
    nixhash::{HashAlgo, NixHash, NixHashWithMode},
    store_path::StorePath,
};
use serde_json::json;
use smol_str::SmolStr;
use std::{
    io,
    path::{Path, PathBuf},
    rc::Rc,
};
use tracing::warn;
use tvix_eval::{ErrorKind, EvalIO, FileType, StdIO};

use crate::{
    blobservice::BlobService,
    directoryservice::{self, DirectoryService},
    import,
    nar::NARCalculationService,
    pathinfoservice::PathInfoService,
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

        directoryservice::traverse_to(&mut self.directory_service, root_node, &sub_path)
    }
}

impl<
        BS: BlobService + Clone,
        DS: DirectoryService + Clone,
        PS: PathInfoService,
        NCS: NARCalculationService,
    > EvalIO for TvixStoreIO<BS, DS, PS, NCS>
{
    fn path_exists(&mut self, path: std::path::PathBuf) -> Result<bool, ErrorKind> {
        if let Ok((store_path, sub_path)) =
            StorePath::from_absolute_path_full(&path.to_string_lossy())
        {
            if self
                .store_path_to_root_node(&store_path, &sub_path)
                .map_err(|e| ErrorKind::IO {
                    path: Some(path.clone()),
                    error: Rc::new(e.into()),
                })?
                .is_some()
            {
                Ok(true)
            } else {
                // As tvix-store doesn't manage /nix/store on the filesystem,
                // we still need to also ask self.std_io here.
                return self.std_io.path_exists(path);
            }
        } else {
            // The store path is no store path, so do regular StdIO.
            self.std_io.path_exists(path)
        }
    }

    fn read_to_string(&mut self, path: std::path::PathBuf) -> Result<String, ErrorKind> {
        if let Ok((store_path, sub_path)) =
            StorePath::from_absolute_path_full(&path.to_string_lossy())
        {
            if let Some(node) = self
                .store_path_to_root_node(&store_path, &sub_path)
                .map_err(|e| ErrorKind::IO {
                    path: Some(path.clone()),
                    error: Rc::new(e.into()),
                })?
            {
                // depending on the node type, treat read_to_string differently
                match node {
                    crate::proto::node::Node::Directory(e) => Err(ErrorKind::IO {
                        path: Some(path),
                        error: Rc::new(std::io::Error::new(
                            // TODO: check what ErrorKind other impls return here
                            std::io::ErrorKind::Other,
                            format!("tried to read directory {:?} to string", e),
                        )),
                    }),
                    crate::proto::node::Node::File(e) => {
                        let digest: [u8; 32] = e
                            .digest
                            .try_into()
                            .map_err(|_e| {
                                crate::Error::StorageError("invalid digest length".to_string())
                            })
                            .map_err(|e| ErrorKind::IO {
                                path: Some(path.clone()),
                                error: Rc::new(e.into()),
                            })?;

                        let digest_b64 = data_encoding::BASE64.encode(&digest);

                        let reader = {
                            let resp = self.blob_service.open_read(&digest).map_err(|e| {
                                ErrorKind::IO {
                                    path: Some(path.clone()),
                                    error: Rc::new(e.into()),
                                }
                            })?;
                            if let Some(reader) = resp {
                                reader
                            } else {
                                Err(ErrorKind::IO {
                                    path: Some(path.clone()),
                                    error: Rc::new(io::Error::new(
                                        io::ErrorKind::Other,
                                        format!("blob {} not found", &digest_b64),
                                    )),
                                })?
                            }
                        };

                        io::read_to_string(reader).map_err(|e| ErrorKind::IO {
                            path: Some(path.clone()),
                            error: Rc::new(e.into()),
                        })
                    }
                    crate::proto::node::Node::Symlink(_) => Err(ErrorKind::TvixBug {
                        msg: "read_to_string for symlinks is unsupported",
                        metadata: Some(Rc::new(json!({
                            "path": format!("{:?}", path),
                        }))),
                    }),
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

    fn read_dir(
        &mut self,
        path: std::path::PathBuf,
    ) -> Result<Vec<(SmolStr, FileType)>, ErrorKind> {
        if let Ok((store_path, sub_path)) =
            StorePath::from_absolute_path_full(&path.to_string_lossy())
        {
            if let Some(node) = self
                .store_path_to_root_node(&store_path, &sub_path)
                .map_err(|e| ErrorKind::IO {
                    path: Some(path.clone()),
                    error: Rc::new(e.into()),
                })?
            {
                match node {
                    crate::proto::node::Node::Directory(e) => {
                        // fetch the Directory itself.
                        let digest: [u8; 32] = e
                            .digest
                            .try_into()
                            .map_err(|_e| {
                                crate::Error::StorageError("invalid digest length".to_string())
                            })
                            .map_err(|e| ErrorKind::IO {
                                path: Some(path.clone()),
                                error: Rc::new(e.into()),
                            })?;

                        let digest_b64 = data_encoding::BASE64.encode(&digest);

                        if let Some(directory) =
                            self.directory_service
                                .get(&digest)
                                .map_err(|e| ErrorKind::IO {
                                    path: Some(path.clone()),
                                    error: Rc::new(e.into()),
                                })?
                        {
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
                            warn!("directory {} does not exist", &digest_b64);
                            Err(crate::Error::StorageError(format!(
                                "directory {} does not exist",
                                digest_b64
                            )))
                            .map_err(|e| ErrorKind::IO {
                                path: Some(path),
                                error: Rc::new(e.into()),
                            })
                        }
                    }
                    crate::proto::node::Node::File(e) => Err(ErrorKind::IO {
                        path: Some(path),
                        error: Rc::new(std::io::Error::new(
                            // TODO: check what ErrorKind other impls return here
                            std::io::ErrorKind::Other,
                            format!("tried to readdir path {:?}, which is file", e),
                        )),
                    }),
                    crate::proto::node::Node::Symlink(_e) => Err(ErrorKind::TvixBug {
                        msg: "read_dir for symlinks is unsupported",
                        metadata: Some(Rc::new(json!({
                            "path": format!("{:?}", path),
                        }))),
                    }),
                }
            } else {
                self.std_io.read_dir(path)
            }
        } else {
            self.std_io.read_dir(path)
        }
    }

    fn import_path(&mut self, path: &std::path::Path) -> Result<std::path::PathBuf, ErrorKind> {
        // Call [import::import_path], which will walk over the given path and return a root_node.
        let root_node =
            import::ingest_path(&mut self.blob_service, &mut self.directory_service, path)
                .expect("error during import_path");

        // Render the NAR
        let calculate_nar_response = self
            .nar_calculation_service
            .calculate_nar(&root_node)
            .expect("error during nar calculation"); // TODO: handle error

        let nar_hash_with_mode = NixHashWithMode::Recursive(NixHash::new(
            HashAlgo::Sha256,
            calculate_nar_response.nar_sha256.to_vec(),
        ));

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
                path.file_name()
                    .expect("path must not be ..")
                    .to_str()
                    .expect("path must be valid unicode"),
                // Note the derivation_or_fod_hash argument is *unused* for FODs, so it doesn't matter what we pass here.
                &NixHash::new(HashAlgo::Sha256, vec![]),
            )
            .expect("error during output path calculation");

            drv.outputs.get("out").unwrap().path.to_string()
        };

        // assemble a new root_node with a name that is derived from the nar hash.
        // TODO: refactor into rename_node function
        let renamed_root_node = {
            // Now we have the output path, but this still is a string. We're only
            // interested in the last leaf, so extract that.
            let name = PathBuf::from(&output_path)
                .file_name()
                .unwrap()
                .to_str()
                .unwrap()
                .to_string();

            // TODO: ungracefully fails if we don't have valid unicode here.

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

        // put into PathInfo service.
        // There's no reference scanning on paths imported like this.
        self.path_info_service
            .put(crate::proto::PathInfo {
                node: Some(crate::proto::Node {
                    node: Some(renamed_root_node),
                }),
                references: vec![],
                narinfo: Some(crate::proto::NarInfo {
                    nar_size: calculate_nar_response.nar_size,
                    nar_sha256: calculate_nar_response.nar_sha256,
                    signatures: vec![],
                    reference_names: vec![],
                    // TODO: narinfo for talosctl.src contains `CA: fixed:r:sha256:1x13j5hy75221bf6kz7cpgld9vgic6bqx07w5xjs4pxnksj6lxb6`
                    // do we need this anywhere?
                }),
            })
            .unwrap(); // TODO: error handling

        Ok(PathBuf::from(output_path))
    }

    fn store_dir(&self) -> Option<String> {
        Some("/nix/store".to_string())
    }
}
