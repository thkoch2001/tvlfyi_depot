use std::sync::Arc;

use crate::{
    blobservice::{BlobService, MemoryBlobService},
    directoryservice::{DirectoryService, MemoryDirectoryService},
    pathinfoservice::{MemoryPathInfoService, PathInfoService},
};

pub fn gen_blob_service() -> Arc<dyn BlobService + Send + Sync + 'static> {
    Arc::new(MemoryBlobService::default())
}

pub fn gen_directory_service() -> Arc<dyn DirectoryService + Send + Sync + 'static> {
    Arc::new(MemoryDirectoryService::default())
}

pub fn gen_pathinfo_service(
    blob_service: Arc<dyn BlobService + Send + Sync + 'static>,
    directory_service: Arc<dyn DirectoryService + Send + Sync + 'static>,
) -> Arc<dyn PathInfoService + Send + Sync + 'static> {
    Arc::new(MemoryPathInfoService::new(blob_service, directory_service))
}
