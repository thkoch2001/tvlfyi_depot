use std::sync::Arc;

use async_trait::async_trait;
use nix_compat::nix_daemon::containers::OptionWithPresence;
use nix_compat::nix_daemon::{types, NixDaemonIO};
use nix_compat::store_path::StorePath;
use tracing::instrument;
use tvix_store::pathinfoservice::PathInfoService;

pub struct TvixDaemon {
    path_info_service: Arc<dyn PathInfoService>,
}

impl TvixDaemon {
    pub fn new(path_info_service: Arc<dyn PathInfoService>) -> Self {
        Self { path_info_service }
    }
}

#[async_trait]
impl NixDaemonIO for TvixDaemon {
    #[instrument(skip(self), ret, err)]
    async fn query_path_info(
        &self,
        path: &types::StorePath,
    ) -> std::io::Result<OptionWithPresence<types::UnkeyedValidPathInfo>> {
        let parsed = StorePath::<String>::from_absolute_path(path.path.as_bytes())
            .map_err(|e| std::io::Error::other(format!("{e:?}")))?;

        let result = self.path_info_service.get(*parsed.digest()).await?;
        Ok(result.map(Into::into).into())
    }
}
