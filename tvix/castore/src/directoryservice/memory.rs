use crate::{B3Digest, Error};
use futures::stream::BoxStream;
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;
use tonic::async_trait;
use tracing::{instrument, warn};

use super::utils::traverse_directory;
use super::{Directory, DirectoryPutter, DirectoryService, SimplePutter};
use crate::composition::{CompositionContext, ServiceBuilder};
use crate::proto;

#[derive(Clone, Default)]
pub struct MemoryDirectoryService {
    instance_name: String,
    db: Arc<RwLock<HashMap<B3Digest, proto::Directory>>>,
}

#[async_trait]
impl DirectoryService for MemoryDirectoryService {
    #[instrument(skip(self, digest), err, fields(directory.digest = %digest, instance_name=%self.instance_name))]
    async fn get(&self, digest: &B3Digest) -> Result<Option<Directory>, Error> {
        let db = self.db.read().await;

        match db.get(digest) {
            // The directory was not found, return
            None => Ok(None),

            // The directory was found, try to parse the data as Directory message
            Some(directory) => {
                // Validate the retrieved Directory indeed has the
                // digest we expect it to have, to detect corruptions.
                let actual_digest = directory.digest();
                if actual_digest != *digest {
                    return Err(Error::StorageError(format!(
                        "requested directory with digest {}, but got {}",
                        digest, actual_digest
                    )));
                }

                Ok(Some(directory.clone().try_into().map_err(|e| {
                    crate::Error::StorageError(format!("corrupted directory: {}", e))
                })?))
            }
        }
    }

    #[instrument(skip(self, directory), err, fields(directory.digest = %directory.digest(), instance_name=%self.instance_name))]
    async fn put(&self, directory: Directory) -> Result<B3Digest, Error> {
        let digest = directory.digest();

        // store it
        let mut db = self.db.write().await;
        db.insert(digest.clone(), directory.into());

        Ok(digest)
    }

    #[instrument(skip_all, fields(directory.digest = %root_directory_digest, instance_name=%self.instance_name))]
    fn get_recursive(
        &self,
        root_directory_digest: &B3Digest,
    ) -> BoxStream<'static, Result<Directory, Error>> {
        traverse_directory(self.clone(), root_directory_digest)
    }

    #[instrument(skip_all, fields(instance_name=%self.instance_name))]
    fn put_multiple_start(&self) -> Box<(dyn DirectoryPutter + 'static)>
    where
        Self: Clone,
    {
        Box::new(SimplePutter::new(self.clone()))
    }
}

#[derive(serde::Deserialize, Debug)]
#[serde(deny_unknown_fields)]
pub struct MemoryDirectoryServiceConfig {}

impl TryFrom<url::Url> for MemoryDirectoryServiceConfig {
    type Error = Box<dyn std::error::Error + Send + Sync>;
    fn try_from(url: url::Url) -> Result<Self, Self::Error> {
        // memory doesn't support host or path in the URL.
        if url.has_host() || !url.path().is_empty() {
            return Err(Error::StorageError("invalid url".to_string()).into());
        }
        Ok(MemoryDirectoryServiceConfig {})
    }
}

#[async_trait]
impl ServiceBuilder for MemoryDirectoryServiceConfig {
    type Output = dyn DirectoryService;
    async fn build<'a>(
        &'a self,
        instance_name: &str,
        _context: &CompositionContext,
    ) -> Result<Arc<dyn DirectoryService>, Box<dyn std::error::Error + Send + Sync + 'static>> {
        Ok(Arc::new(MemoryDirectoryService {
            instance_name: instance_name.to_string(),
            db: Default::default(),
        }))
    }
}
