//! This module provides an implementation of EvalIO.
//!
//! It can be used by the tvix evalutator to talk to a tvix store.

use nix_compat::{
    derivation::{Derivation, Output},
    nixhash::{HashAlgo, NixHash, NixHashWithMode},
    store_path::StorePath,
};
use smol_str::SmolStr;
use std::{io, path::PathBuf};
use tracing::warn;
use tvix_eval::{ErrorKind, EvalIO, FileType, StdIO};

use crate::{
    blobservice::BlobService, directoryservice::DirectoryService, import,
    nar::NARCalculationService, pathinfoservice::PathInfoService,
};

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

    /// with a given [StorePath], look up the root_node, or return None if not found.
    fn store_path_to_root_node(&self, store_path: &StorePath) -> Option<crate::proto::Node> {
        let resp = self
            .path_info_service
            .get(crate::proto::get_path_info_request::ByWhat::ByOutputHash(
                store_path.digest.to_vec(),
            ))
            // TODO: proper error handling?
            .expect("pathinfo service returned error");

        match resp {
            Some(path_info) => {
                if path_info.node.is_none() {
                    warn!(
                        "returned PathInfo {:?} node is None, this shouldn't happen.",
                        &path_info
                    );
                }
                path_info.node
            }
            None => None,
        }
    }
}

impl<
        BS: BlobService + Clone,
        DS: DirectoryService + Clone,
        PS: PathInfoService,
        NCS: NARCalculationService,
    > EvalIO for TvixStoreIO<BS, DS, PS, NCS>
{
    /// try to parse the full path as a NixPath, query PathInfoService and traverse the store,
    /// otherwise fall back to StdIO (regular filesystem path)
    fn path_exists(&self, path: std::path::PathBuf) -> Result<bool, ErrorKind> {
        if let Ok(store_path) = StorePath::from_absolute_path(&path.to_string_lossy()) {
            match self.store_path_to_root_node(&store_path) {
                None => {
                    // if path is a syntactically valid store path, but PathInfoService returns none,
                    // it doesn't exist.
                    Ok(false)
                }
                Some(root_node) => {
                    // if we get back a root_node, there might still be some
                    // trailing path here. make sure to traverse this and check
                    // if the final path exists.

                    todo!()
                }
            }
        } else {
            // The store path is no store path, so do regular StdIO.
            self.std_io.path_exists(path)
        }
    }

    /// try to parse the full path as a NixPath, query PathInfoService and traverse the store,
    /// otherwise fall back to StdIO (regular filesystem path)
    fn read_to_string(&self, path: std::path::PathBuf) -> Result<String, ErrorKind> {
        todo!()
    }

    // parse the (full) path info a NixPath, check for the PathInfo to be present
    // walk from there to the directory and return its items.
    fn read_dir(&self, path: std::path::PathBuf) -> Result<Vec<(SmolStr, FileType)>, ErrorKind> {
        todo!()
    }

    // TODO: can we make this &mut self, so we don't need to Clone BS and DS?
    fn import_path(&self, path: &std::path::Path) -> Result<std::path::PathBuf, ErrorKind> {
        // Call [import::import_path], which will walk over the given path and return a root_node.
        let root_node = import::import_path(
            &mut self.blob_service.clone(),
            &mut self.directory_service.clone(),
            path,
        )
        .expect("error during import_path");

        // Render the NAR
        let nar_hash = NixHashWithMode::Recursive(NixHash::new(
            HashAlgo::Sha256,
            self.nar_calculation_service
                .calculate_nar(&root_node)
                .expect("error during nar calculation")
                .nar_sha256,
        ));

        let mut drv = Derivation::default();
        drv.outputs.insert(
            "out".to_string(),
            Output {
                path: "".to_string(),
                hash_with_mode: Some(nar_hash),
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

        let output_path = drv.outputs.get("out").unwrap().path.to_string();

        Ok(PathBuf::from(output_path))
    }
}
