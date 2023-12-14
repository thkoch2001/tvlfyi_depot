use std::{ops::Deref, pin::Pin};

use futures::{Stream, StreamExt};
use tonic::async_trait;
use tvix_castore::{proto::node::Node, Error};

use crate::pathinfoservice::PathInfoService;

#[async_trait]
pub trait Lookup: Send + Sync {
    async fn get(&self, digest: [u8; 20]) -> Result<Option<Node>, Error>;

    fn list(&self) -> Pin<Box<dyn Stream<Item = Result<Node, Error>> + Send>>;
}

#[async_trait]
impl<T> Lookup for T
where
    T: Deref<Target = dyn PathInfoService> + Send + Sync,
{
    async fn get(&self, digest: [u8; 20]) -> Result<Option<Node>, Error> {
        Ok(self
            .deref()
            .get(digest)
            .await?
            .map(|path_info| path_info.node.unwrap().node.unwrap()))
    }

    fn list(&self) -> Pin<Box<dyn Stream<Item = Result<Node, Error>> + Send>> {
        Box::pin(
            self.deref()
                .list()
                .map(|result| result.map(|path_info| path_info.node.unwrap().node.unwrap())),
        )
    }
}
