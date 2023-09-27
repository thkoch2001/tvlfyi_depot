mod pathinfo;

use std::{path::Path, pin::Pin};

use futures::Stream;
use tonic::async_trait;
use tvix_castore::proto as castorepb;

#[async_trait]
trait LookupService {
    async fn lookup_root(&self, path: &Path) -> castorepb::node::Node;

    fn list_root(&self, path: &Path) -> Pin<Box<dyn Stream<Item = castorepb::node::Node>>>;
}
