pub mod pathinfo;

use std::{path::Path, pin::Pin};

use crate::proto as storepb;
use futures::Stream;
use tonic::async_trait;
use tvix_castore::Error;

#[async_trait]
pub trait LookupService: Send + Sync {
    async fn lookup(&self, path: &Path) -> Result<Option<storepb::lookup_node::Contents>, Error>;

    // FIXME: Support listing!!
    // fn list(
    //     &self,
    //     path: &Path,
    // ) -> Pin<Box<dyn Stream<Item = Result<storepb::Entry, Error>> + Send>>;
}
