use std::{path::Path, pin::Pin};

use futures::Stream;
use tonic::async_trait;

use super::LookupService;
use crate::pathinfoservice::PathInfoService;
use tvix_castore::proto as castorepb;

pub struct PathInfoLookupService {
    path_info_service: Box<dyn PathInfoService>,
}

#[async_trait]
impl LookupService for PathInfoLookupService {
    async fn lookup_root(&self, path: &Path) -> castorepb::node::Node {
        todo!();
    }

    fn list_root(&self, path: &Path) -> Pin<Box<dyn Stream<Item = castorepb::node::Node>>> {
        todo!();
    }
}
