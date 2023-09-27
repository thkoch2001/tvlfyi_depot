use std::path::Path;
use std::sync::Arc;

use super::LookupService;
use crate::pathinfoservice::PathInfoService;
use crate::proto::lookup_node::Contents;
use nix_compat::store_path::StorePath;
use tonic::async_trait;
use tvix_castore::Error;

pub struct PathInfoLookupService {
    path_info_service: Arc<dyn PathInfoService>,
}

impl PathInfoLookupService {
    pub fn new(path_info_service: Arc<dyn PathInfoService>) -> Arc<Self> {
        Arc::new(Self { path_info_service })
    }
}

#[async_trait]
impl LookupService for PathInfoLookupService {
    async fn lookup(&self, path: &Path) -> Result<Option<Contents>, Error> {
        // FIXME: This conversion seems wrong. Path may be UTF-8 or UTF-16 depending on platform,
        // so we can't naively convert from &Path -> &[u8]
        let store_path = match StorePath::from_bytes(path.to_string_lossy().as_bytes()) {
            Err(_) => return Ok(None),
            Ok(store_path) => store_path,
        };

        let path_info = match self.path_info_service.get(store_path.digest).await? {
            None => return Ok(None),
            Some(node) => node,
        };

        Ok(Some(Contents::Canode(path_info.node.unwrap())))
    }
}
