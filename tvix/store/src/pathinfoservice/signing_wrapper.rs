use super::PathInfoService;
use crate::proto::PathInfo;
use futures::stream::BoxStream;
use std::path::Path;
use std::sync::Arc;
use tonic::async_trait;

use tvix_castore::composition::{CompositionContext, ServiceBuilder};

use tvix_castore::Error;

use nix_compat::narinfo::{parse_keypair, SigningKey};
use nix_compat::nixbase32;
use tracing::{instrument, warn};

pub struct SigningPathInfoService<T, S> {
    inner: T,
    signing_key: Arc<SigningKey<S>>,
}

#[async_trait]
impl<T, S> PathInfoService for SigningPathInfoService<T, S>
where
    T: PathInfoService,
    S: ed25519::signature::Signer<ed25519::Signature> + Sync + Send,
{
    #[instrument(level = "trace", skip_all, fields(path_info.digest = nixbase32::encode(&digest)))]
    async fn get(&self, digest: [u8; 20]) -> Result<Option<PathInfo>, Error> {
        self.inner.get(digest).await
    }

    async fn put(&self, path_info: PathInfo) -> Result<PathInfo, Error> {
        let store_path = path_info.validate().map_err(|e| {
            warn!(err=%e, "invalid PathInfo");
            Error::StorageError(e.to_string())
        })?;
        let root_node = path_info.node.clone();
        let mut nar_info = path_info
            .to_narinfo(store_path)
            .ok_or(Error::StorageError("".to_string()))?;
        nar_info.add_signature(self.signing_key.as_ref());
        let mut signed_path_info = PathInfo::from(&nar_info);
        signed_path_info.node = root_node;
        self.inner.put(signed_path_info).await
    }

    fn list(&self) -> BoxStream<'static, Result<PathInfo, Error>> {
        self.inner.list()
    }
}

#[derive(serde::Deserialize)]
pub struct KeyFileSigningPathInfoServiceConfig {
    pub inner: String,
    pub keyfile: Box<Path>,
}

impl TryFrom<url::Url> for KeyFileSigningPathInfoServiceConfig {
    type Error = Box<dyn std::error::Error + Send + Sync>;
    fn try_from(_url: url::Url) -> Result<Self, Self::Error> {
        Err(Error::StorageError(
            "Instantiating a SigningPathInfoService from a url is not supported".into(),
        )
        .into())
    }
}

#[async_trait]
impl ServiceBuilder for KeyFileSigningPathInfoServiceConfig {
    type Output = dyn PathInfoService;
    async fn build<'a>(
        &'a self,
        _instance_name: &str,
        context: &CompositionContext,
    ) -> Result<Arc<dyn PathInfoService>, Box<dyn std::error::Error + Send + Sync + 'static>> {
        let inner = context.resolve(self.inner.clone()).await?;
        let signing_key = Arc::new(
            parse_keypair(tokio::fs::read_to_string(&self.keyfile).await?.trim())
                .map_err(|e| Error::StorageError(e.to_string()))?
                .0,
        );
        Ok(Arc::new(SigningPathInfoService { inner, signing_key }))
    }
}
