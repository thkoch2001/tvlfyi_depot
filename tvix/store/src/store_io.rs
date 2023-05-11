//! This module provides an implementation of EvalIO.
//!
//! It can be used by the tvix evalutator to talk to a tvix store.

use smol_str::SmolStr;
use tvix_eval::{ErrorKind, EvalIO, FileType};

use crate::{
    blobservice::BlobService, directoryservice::DirectoryService, pathinfoservice::PathInfoService,
};

pub struct TvixStoreIO<BS: BlobService, DS: DirectoryService, PS: PathInfoService> {
    blob_service: BS,
    directory_service: DS,
    path_info_service: PS,
}

impl<BS: BlobService, DS: DirectoryService, PS: PathInfoService> TvixStoreIO<BS, DS, PS> {
    pub fn new(blob_service: BS, directory_service: DS, path_info_service: PS) -> Self {
        Self {
            blob_service,
            directory_service,
            path_info_service,
        }
    }
}

impl<BS: BlobService, DS: DirectoryService, PS: PathInfoService> EvalIO
    for TvixStoreIO<BS, DS, PS>
{
    fn path_exists(&self, path: std::path::PathBuf) -> Result<bool, ErrorKind> {
        // parse the (full) path info a NixPath, check for the PathInfo to be present
        todo!()
    }

    fn read_to_string(&self, path: std::path::PathBuf) -> Result<String, ErrorKind> {
        // parse the (full) path info a NixPath, check for the PathInfo to be present
        // walk from there to the file and return its contents
        todo!()
    }

    fn read_dir(&self, path: std::path::PathBuf) -> Result<Vec<(SmolStr, FileType)>, ErrorKind> {
        // parse the (full) path info a NixPath, check for the PathInfo to be present
        // walk from there to the directory mentioned and return its items
        todo!()
    }

    fn import_path(&self, path: &std::path::Path) -> Result<std::path::PathBuf, ErrorKind> {
        // call the import library function, then render as NAR.
        // TODO: make the nar calculation service generic and provide a caching
        // version, so we don't need to pull down the contents again.
        todo!()
    }
}
