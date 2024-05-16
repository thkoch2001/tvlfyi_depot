use futures::stream::BoxStream;
use futures::StreamExt;
use futures::TryFutureExt;
use futures::TryStreamExt;
use tonic::async_trait;
use tracing::{instrument, trace};

use super::{ClosureValidator, DirectoryService, RootToLeavesValidator, SimplePutter};
use crate::directoryservice::DirectoryPutter;
use crate::proto;
use crate::B3Digest;
use crate::Error;

/// Asks near first, if not found, asks far.
/// If found in there, returns it, and *inserts* it into
/// near.
/// There is no negative cache.
/// Inserts and listings are not implemented for now.
#[derive(Clone)]
pub struct Cache<DS1, DS2> {
    near: DS1,
    far: DS2,
}

impl<DS1, DS2> Cache<DS1, DS2> {
    pub fn new(near: DS1, far: DS2) -> Self {
        Self { near, far }
    }
}

#[async_trait]
impl<DS1, DS2> DirectoryService for Cache<DS1, DS2>
where
    DS1: DirectoryService + Clone + 'static,
    DS2: DirectoryService + Clone + 'static,
{
    #[instrument(skip(self, digest), fields(directory.digest = %digest))]
    async fn get(&self, digest: &B3Digest) -> Result<Option<proto::Directory>, Error> {
        match self.near.get(digest).await? {
            Some(directory) => {
                trace!("serving from cache");
                Ok(Some(directory))
            }
            None => {
                trace!("not found in near, asking remote…");

                let mut copy = ClosureValidator::with_order(
                    RootToLeavesValidator::new_with_root_digest(digest.clone()),
                );

                let mut stream = std::pin::pin!(self.far.get_recursive(digest).peekable());
                let root = stream.as_mut().peek().await.cloned().transpose()?;

                while let Some(dir) = stream.try_next().await? {
                    copy.add(dir)
                        .map_err(|e| Error::StorageError(e.to_string()))?;
                }

                copy.validate()
                    .map_err(|e| Error::StorageError(e.to_string()))?;

                let mut put = self.near.put_multiple_start();
                for dir in copy.drain_leaves_to_root() {
                    put.put(dir).await?;
                }
                put.close().await?;

                Ok(root)
            }
        }
    }

    #[instrument(skip_all)]
    async fn put(&self, _directory: proto::Directory) -> Result<B3Digest, Error> {
        Err(Error::StorageError("unimplemented".to_string()))
    }

    //#[instrument(skip_all, fields(directory.digest = %root_directory_digest))]
    fn get_recursive(
        &self,
        root_directory_digest: &B3Digest,
    ) -> BoxStream<'static, Result<proto::Directory, Error>> {
        let near = self.near.clone();
        let far = self.far.clone();
        let digest = root_directory_digest.clone();
        Box::pin(
            (async move {
                let mut stream = near.get_recursive(&digest);
                match stream.try_next().await? {
                    Some(first) => {
                        trace!("serving from cache");
                        Ok(futures::stream::once(async { Ok(first) })
                            .chain(stream)
                            .left_stream())
                    }
                    None => {
                        trace!("not found in near, asking remote…");

                        let mut copy1 = ClosureValidator::with_order(
                            RootToLeavesValidator::new_with_root_digest(digest.clone()),
                        );
                        let mut copy2 = vec![];

                        let mut stream = far.get_recursive(&digest);
                        while let Some(dir) = stream.try_next().await? {
                            copy1
                                .add(dir.clone())
                                .map_err(|e| Error::StorageError(e.to_string()))?;
                            copy2.push(dir);
                        }

                        copy1
                            .validate()
                            .map_err(|e| Error::StorageError(e.to_string()))?;
                        let mut put = near.put_multiple_start();
                        for dir in copy1.drain_leaves_to_root() {
                            put.put(dir).await?;
                        }
                        put.close().await?;

                        Ok(futures::stream::iter(copy2.into_iter().map(Ok)).right_stream())
                    }
                }
            })
            .try_flatten_stream(),
        )
    }

    #[instrument(skip_all)]
    fn put_multiple_start(&self) -> Box<(dyn DirectoryPutter + 'static)> {
        Box::new(SimplePutter::new((*self).clone()))
    }
}
