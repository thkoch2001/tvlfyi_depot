use std::{
    collections::HashMap,
    pin::Pin,
    sync::Arc,
    task::{self, Poll},
};
use tokio::io::{self, AsyncWrite};

use tvix_castore::{blobservice::BlobService, directoryservice::DirectoryService};
use url::Url;

use crate::composition::{
    with_registry, Composition, DeserializeWithRegistry, ServiceBuilder, REG,
};
use crate::nar::{NarCalculationService, SimpleRenderer};
use crate::pathinfoservice::PathInfoService;

#[derive(serde::Deserialize, Default)]
pub struct CompositionConfigs {
    pub blobservices:
        HashMap<String, DeserializeWithRegistry<Box<dyn ServiceBuilder<Output = dyn BlobService>>>>,
    pub directoryservices: HashMap<
        String,
        DeserializeWithRegistry<Box<dyn ServiceBuilder<Output = dyn DirectoryService>>>,
    >,
    pub pathinfoservices: HashMap<
        String,
        DeserializeWithRegistry<Box<dyn ServiceBuilder<Output = dyn PathInfoService>>>,
    >,
}

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

    /// URL to a PathInfoService that's considered "remote".
    /// If set, the other one is considered "local", and a "cache" for the
    /// "remote" one.
    #[arg(long, env)]
    remote_path_info_service_addr: Option<String>,

    /// Path to a TOML file describing the way the services should be composed
    /// Experimental because the format is not final.
    /// If specified, the other service addrs are ignored.
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

    #[arg(long, env)]
    remote_path_info_service_addr: Option<String>,

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

    #[arg(long, env)]
    remote_path_info_service_addr: Option<String>,

    #[arg(long, env)]
    experimental_store_composition: Option<String>,
}

impl From<ServiceUrlsGrpc> for ServiceUrls {
    fn from(urls: ServiceUrlsGrpc) -> ServiceUrls {
        ServiceUrls {
            blob_service_addr: urls.blob_service_addr,
            directory_service_addr: urls.directory_service_addr,
            path_info_service_addr: urls.path_info_service_addr,
            remote_path_info_service_addr: urls.remote_path_info_service_addr,
            experimental_store_composition: urls.experimental_store_composition,
        }
    }
}

impl From<ServiceUrlsMemory> for ServiceUrls {
    fn from(urls: ServiceUrlsMemory) -> ServiceUrls {
        ServiceUrls {
            blob_service_addr: urls.blob_service_addr,
            directory_service_addr: urls.directory_service_addr,
            path_info_service_addr: urls.path_info_service_addr,
            remote_path_info_service_addr: urls.remote_path_info_service_addr,
            experimental_store_composition: urls.experimental_store_composition,
        }
    }
}

pub async fn addrs_to_configs(
    urls: impl Into<ServiceUrls>,
) -> Result<CompositionConfigs, Box<dyn std::error::Error + Send + Sync>> {
    let urls: ServiceUrls = urls.into();

    if let Some(conf_path) = urls.experimental_store_composition {
        let conf_text = tokio::fs::read_to_string(conf_path).await?;
        return Ok(with_registry(&REG, || toml::from_str(&conf_text))?);
    }

    let mut configs: CompositionConfigs = Default::default();

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

    // if remote_path_info_service_addr has been specified,
    // update path_info_service to point to a cache combining the two.
    if let Some(addr) = urls.remote_path_info_service_addr {
        use crate::composition::{with_registry, DeserializeWithRegistry, REG};
        use crate::pathinfoservice::CachePathInfoServiceConfig;

        let remote_url = url::Url::parse(&addr)?;
        let remote_config = with_registry(&REG, || remote_url.try_into())?;

        let local = configs.pathinfoservices.insert(
            "default".into(),
            DeserializeWithRegistry(Box::new(CachePathInfoServiceConfig {
                near: "local".into(),
                far: "remote".into(),
            })),
        );
        configs
            .pathinfoservices
            .insert("local".into(), local.unwrap());
        configs
            .pathinfoservices
            .insert("remote".into(), remote_config);
    }

    Ok(configs)
}

/// Construct the store handles from their addrs.
pub async fn construct_services(
    urls: impl Into<ServiceUrls>,
) -> Result<
    (
        Arc<dyn BlobService>,
        Arc<dyn DirectoryService>,
        Arc<dyn PathInfoService>,
        Box<dyn NarCalculationService>,
    ),
    Box<dyn std::error::Error + Send + Sync>,
> {
    let configs = addrs_to_configs(urls).await?;
    construct_services_from_configs(configs).await
}

/// Construct the store handles from their addrs.
pub async fn construct_services_from_configs(
    configs: CompositionConfigs,
) -> Result<
    (
        Arc<dyn BlobService>,
        Arc<dyn DirectoryService>,
        Arc<dyn PathInfoService>,
        Box<dyn NarCalculationService>,
    ),
    Box<dyn std::error::Error + Send + Sync>,
> {
    let mut comp = Composition::default();

    comp.extend(configs.blobservices);
    comp.extend(configs.directoryservices);
    comp.extend(configs.pathinfoservices);

    let blob_service: Arc<dyn BlobService> = comp.build("default").await?;
    let directory_service: Arc<dyn DirectoryService> = comp.build("default").await?;
    let path_info_service: Arc<dyn PathInfoService> = comp.build("default").await?;

    // HACK: The grpc client also implements NarCalculationService, and we
    // really want to use it (otherwise we'd need to fetch everything again for hashing).
    // Until we revamped store composition and config, detect this special case here.
    let nar_calculation_service: Box<dyn NarCalculationService> = path_info_service
        .nar_calculation_service()
        .unwrap_or_else(|| {
            Box::new(SimpleRenderer::new(
                blob_service.clone(),
                directory_service.clone(),
            ))
        });

    Ok((
        blob_service,
        directory_service,
        path_info_service,
        nar_calculation_service,
    ))
}

/// The inverse of [tokio_util::io::SyncIoBridge].
/// Don't use this with anything that actually does blocking I/O.
pub struct AsyncIoBridge<T>(pub T);

impl<W: std::io::Write + Unpin> AsyncWrite for AsyncIoBridge<W> {
    fn poll_write(
        self: Pin<&mut Self>,
        _cx: &mut task::Context<'_>,
        buf: &[u8],
    ) -> Poll<io::Result<usize>> {
        Poll::Ready(self.get_mut().0.write(buf))
    }

    fn poll_flush(self: Pin<&mut Self>, _cx: &mut task::Context<'_>) -> Poll<io::Result<()>> {
        Poll::Ready(self.get_mut().0.flush())
    }

    fn poll_shutdown(
        self: Pin<&mut Self>,
        _cx: &mut task::Context<'_>,
    ) -> Poll<Result<(), io::Error>> {
        Poll::Ready(Ok(()))
    }
}
