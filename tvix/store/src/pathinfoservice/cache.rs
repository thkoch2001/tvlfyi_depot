use std::sync::Arc;

use futures::stream::BoxStream;
use nix_compat::nixbase32;
use tonic::async_trait;
use tracing::{debug, instrument};
use tvix_castore::composition::{CompositionContext, ServiceBuilder};
use tvix_castore::Error;

use super::{PathInfo, PathInfoService};

/// Asks near first, if not found, asks far.
/// If found in there, returns it, and *inserts* it into
/// near.
/// There is no negative cache.
/// Inserts and listings are not implemented for now.
pub struct Cache<PS1, PS2> {
    instance_name: String,
    near: PS1,
    far: PS2,
}

impl<PS1, PS2> Cache<PS1, PS2> {
    pub fn new(instance_name: String, near: PS1, far: PS2) -> Self {
        Self {
            instance_name,
            near,
            far,
        }
    }
}

#[async_trait]
impl<PS1, PS2> PathInfoService for Cache<PS1, PS2>
where
    PS1: PathInfoService,
    PS2: PathInfoService,
{
    #[instrument(level = "trace", skip_all, fields(path_info.digest = nixbase32::encode(&digest), instance_name = %self.instance_name))]
    async fn get(&self, digest: [u8; 20]) -> Result<Option<PathInfo>, Error> {
        match self.near.get(digest).await? {
            Some(path_info) => {
                debug!("serving from cache");
                Ok(Some(path_info))
            }
            None => {
                debug!("not found in near, asking remote…");
                match self.far.get(digest).await? {
                    None => Ok(None),
                    Some(path_info) => {
                        debug!("found in remote, adding to cache");
                        self.near.put(path_info.clone()).await?;
                        Ok(Some(path_info))
                    }
                }
            }
        }
    }

    async fn put(&self, _path_info: PathInfo) -> Result<PathInfo, Error> {
        Err(Error::StorageError("unimplemented".to_string()))
    }

    fn list(&self) -> BoxStream<'static, Result<PathInfo, Error>> {
        Box::pin(tokio_stream::once(Err(Error::StorageError(
            "unimplemented".to_string(),
        ))))
    }
}

#[derive(serde::Deserialize)]
pub struct CacheConfig {
    pub near: String,
    pub far: String,
}

impl TryFrom<url::Url> for CacheConfig {
    type Error = Box<dyn std::error::Error + Send + Sync>;
    fn try_from(_url: url::Url) -> Result<Self, Self::Error> {
        Err(Error::StorageError(
            "Instantiating a CombinedPathInfoService from a url is not supported".into(),
        )
        .into())
    }
}

#[async_trait]
impl ServiceBuilder for CacheConfig {
    type Output = dyn PathInfoService;
    async fn build<'a>(
        &'a self,
        instance_name: &str,
        context: &CompositionContext,
    ) -> Result<Arc<dyn PathInfoService>, Box<dyn std::error::Error + Send + Sync + 'static>> {
        let (near, far) = futures::join!(
            context.resolve::<Self::Output>(self.near.clone()),
            context.resolve::<Self::Output>(self.far.clone())
        );
        Ok(Arc::new(Cache {
            instance_name: instance_name.to_string(),
            near: near?,
            far: far?,
        }))
    }
}

#[cfg(test)]
mod test {
    use std::num::NonZeroUsize;

    use crate::{
        pathinfoservice::{LruPathInfoService, MemoryPathInfoService, PathInfoService},
        tests::fixtures::PATH_INFO,
    };

    /// Helper function setting up an instance of a "far" and "near"
    /// PathInfoService.
    async fn create_pathinfoservice() -> super::Cache<LruPathInfoService, MemoryPathInfoService> {
        // Create an instance of a "far" PathInfoService.
        let far = MemoryPathInfoService::default();

        // … and an instance of a "near" PathInfoService.
        let near = LruPathInfoService::with_capacity("test".into(), NonZeroUsize::new(1).unwrap());

        // create a Pathinfoservice combining the two and return it.
        super::Cache::new("test".into(), near, far)
    }

    /// Getting from the far backend is gonna insert it into the near one.
    #[tokio::test]
    async fn test_populate_cache() {
        let svc = create_pathinfoservice().await;

        // query the PathInfo, things should not be there.
        assert!(svc
            .get(*PATH_INFO.store_path.digest())
            .await
            .unwrap()
            .is_none());

        // insert it into the far one.
        svc.far.put(PATH_INFO.clone()).await.unwrap();

        // now try getting it again, it should succeed.
        assert_eq!(
            Some(PATH_INFO.clone()),
            svc.get(*PATH_INFO.store_path.digest()).await.unwrap()
        );

        // peek near, it should now be there.
        assert_eq!(
            Some(PATH_INFO.clone()),
            svc.near.get(*PATH_INFO.store_path.digest()).await.unwrap()
        );
    }
}
