use std::{ops::Deref, pin::Pin};

use futures::{Stream, StreamExt};
use nix_compat::store_path::StorePath;
use tonic::async_trait;
use tvix_castore::{proto::node::Node, Error};

use crate::pathinfoservice::PathInfoService;

#[async_trait]
pub trait RootNodes: Send + Sync {
    async fn get_by_basename(&self, name: &[u8]) -> Result<Option<Node>, Error>;

    fn list(&self) -> Pin<Box<dyn Stream<Item = Result<Node, Error>> + Send>>;
}

#[async_trait]
impl<T> RootNodes for T
where
    T: Deref<Target = dyn PathInfoService> + Send + Sync,
{
    async fn get_by_basename(&self, name: &[u8]) -> Result<Option<Node>, Error> {
        let Ok(store_path) = StorePath::from_bytes(name) else {
            return Ok(None);
        };

        Ok(self
            .deref()
            .get(*store_path.digest())
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
