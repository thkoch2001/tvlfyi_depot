use super::CompositionConfigs;
use crate::utils::{with_registry, REG};

use url::Url;

/// A bunch of service-related args shared between many clap commands, to be
/// included with `#[clap(flatten)]`.
///
/// Make sure to set a doc comment on the struct including this, since it shows
/// this doc comment on the help page otherwise.
#[derive(clap::Parser, Clone)]
pub struct ServiceUrls {
    #[arg(
        long,
        env,
        default_value = "objectstore+file:///var/lib/tvix-store/blobs.object_store"
    )]
    blob_service_addr: String,

    #[arg(
        long,
        env,
        default_value = "sled:///var/lib/tvix-store/directories.sled"
    )]
    directory_service_addr: String,

    #[arg(long, env, default_value = "sled:///var/lib/tvix-store/pathinfo.sled")]
    path_info_service_addr: String,

    /// Path to a TOML file describing the way the services should be composed
    /// Experimental because the format is not final.
    /// If specified, the other service addrs are ignored.
    #[cfg(feature = "xp-store-composition")]
    #[arg(long, env)]
    experimental_store_composition: Option<String>,
}

/// like ServiceUrls, but with different clap defaults
#[derive(clap::Parser, Clone)]
pub struct ServiceUrlsGrpc {
    #[arg(long, env, default_value = "grpc+http://[::1]:8000")]
    blob_service_addr: String,

    #[arg(long, env, default_value = "grpc+http://[::1]:8000")]
    directory_service_addr: String,

    #[arg(long, env, default_value = "grpc+http://[::1]:8000")]
    path_info_service_addr: String,

    /// Path to a TOML file describing the way the services should be composed
    /// Experimental because the format is not final.
    /// If specified, the other service addrs are ignored.
    #[cfg(feature = "xp-store-composition")]
    #[arg(long, env)]
    experimental_store_composition: Option<String>,
}

/// like ServiceUrls, but with different clap defaults
#[derive(clap::Parser, Clone)]
pub struct ServiceUrlsMemory {
    #[arg(long, env, default_value = "memory://")]
    blob_service_addr: String,

    #[arg(long, env, default_value = "memory://")]
    directory_service_addr: String,

    #[arg(long, env, default_value = "memory://")]
    path_info_service_addr: String,

    /// Path to a TOML file describing the way the services should be composed
    /// Experimental because the format is not final.
    /// If specified, the other service addrs are ignored.
    #[cfg(feature = "xp-store-composition")]
    #[arg(long, env)]
    experimental_store_composition: Option<String>,
}

impl From<ServiceUrlsGrpc> for ServiceUrls {
    fn from(urls: ServiceUrlsGrpc) -> ServiceUrls {
        ServiceUrls {
            blob_service_addr: urls.blob_service_addr,
            directory_service_addr: urls.directory_service_addr,
            path_info_service_addr: urls.path_info_service_addr,
        }
    }
}

impl From<ServiceUrlsMemory> for ServiceUrls {
    fn from(urls: ServiceUrlsMemory) -> ServiceUrls {
        ServiceUrls {
            blob_service_addr: urls.blob_service_addr,
            directory_service_addr: urls.directory_service_addr,
            path_info_service_addr: urls.path_info_service_addr,
        }
    }
}

pub async fn addrs_to_configs(
    urls: impl Into<ServiceUrls>,
) -> Result<CompositionConfigs, Box<dyn std::error::Error + Send + Sync>> {
    let urls: ServiceUrls = urls.into();
    let mut configs: CompositionConfigs = Default::default();

    #[cfg(feature = "xp-store-composition")]
    if let Some(conf_path) = urls.experimental_store_composition {
        let conf_text = tokio::fs::read_to_string(conf_path).await?;
        return Ok(with_registry(&REG, || toml::from_str(&conf_text))?);
    }

    let blob_service_url = Url::parse(&urls.blob_service_addr)?;
    let directory_service_url = Url::parse(&urls.directory_service_addr)?;
    let path_info_service_url = Url::parse(&urls.path_info_service_addr)?;

    configs.blobservices.insert(
        "default".into(),
        with_registry(&REG, || blob_service_url.try_into())?,
    );
    configs.directoryservices.insert(
        "default".into(),
        with_registry(&REG, || directory_service_url.try_into())?,
    );
    configs.pathinfoservices.insert(
        "default".into(),
        with_registry(&REG, || path_info_service_url.try_into())?,
    );

    Ok(configs)
}
