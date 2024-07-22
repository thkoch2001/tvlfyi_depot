//! This module provides a [PathInfoService] implementation that signs narinfos

use super::PathInfoService;
use crate::proto::PathInfo;
use futures::stream::BoxStream;
use std::path::PathBuf;
use std::sync::Arc;
use tonic::async_trait;

use tvix_castore::composition::{CompositionContext, ServiceBuilder};

use tvix_castore::Error;

use nix_compat::narinfo::{parse_keypair, SigningKey};
use nix_compat::nixbase32;
use tracing::{instrument, warn};

/// Wraps around an inner [PathInfoService].
/// When put is called, extracts the underlying nar info and signs it using a [SigningKey].
/// The reconstructed [PathInfo is the put into the inner [PathInfoService].
pub struct SigningPathInfoService<T, S> {
    /// The inner [PathInfoService]
    inner: T,
    signing_key: Arc<SigningKey<S>>,
}

impl<T, S> SigningPathInfoService<T, S> {
    pub fn new(inner: T, signing_key: Arc<SigningKey<S>>) -> Self {
        Self { inner, signing_key }
    }
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
        let mut nar_info = path_info.to_narinfo(store_path).ok_or(Error::StorageError(
            "Can't render narinfo to create a signature".to_string(),
        ))?;
        nar_info.add_signature(self.signing_key.as_ref());
        let mut signed_path_info = PathInfo::from(&nar_info);
        signed_path_info.node = root_node;
        self.inner.put(signed_path_info).await
    }

    fn list(&self) -> BoxStream<'static, Result<PathInfo, Error>> {
        self.inner.list()
    }
}

/// [ServiceConfig] implementation that builds a [SigningPathInfoService] that signs narinfos using
/// a keyfile
#[derive(serde::Deserialize)]
pub struct KeyFileSigningPathInfoServiceConfig {
    pub inner: String,
    pub keyfile: PathBuf,
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
