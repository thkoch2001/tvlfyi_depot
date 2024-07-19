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

/// Construct the store handles from their addrs.
pub async fn construct_services(
    blob_service_addr: impl AsRef<str>,
    directory_service_addr: impl AsRef<str>,
    path_info_service_addr: impl AsRef<str>,
    remote_path_info_service_addr: Option<String>,
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

    let blob_service_url = Url::parse(blob_service_addr.as_ref())?;
    let directory_service_url = Url::parse(directory_service_addr.as_ref())?;
    let path_info_service_url = Url::parse(path_info_service_addr.as_ref())?;
    let blob_service_config: DeserializeWithRegistry<
        Box<dyn ServiceBuilder<Output = dyn BlobService>>,
    > = with_registry(&REG, || blob_service_url.try_into())?;
    let directory_service_config: DeserializeWithRegistry<
        Box<dyn ServiceBuilder<Output = dyn DirectoryService>>,
    > = with_registry(&REG, || directory_service_url.try_into())?;
    let path_info_service_config: DeserializeWithRegistry<
        Box<dyn ServiceBuilder<Output = dyn PathInfoService>>,
    > = with_registry(&REG, || path_info_service_url.try_into())?;

    comp.extend(std::iter::once(("default".into(), blob_service_config)));
    comp.extend(std::iter::once((
        "default".into(),
        directory_service_config,
    )));

    if let Some(addr) = remote_path_info_service_addr {
        use crate::pathinfoservice::CachePathInfoServiceConfig;

        let mut path_info_configs = HashMap::new();
        path_info_configs.insert("local".into(), path_info_service_config);
        let remote_url = Url::parse(&addr)?;
        let remote_config: DeserializeWithRegistry<
            Box<dyn ServiceBuilder<Output = dyn PathInfoService>>,
        > = with_registry(&REG, || remote_url.try_into())?;
        path_info_configs.insert("remote".into(), remote_config);
        path_info_configs.insert(
            "default".into(),
            DeserializeWithRegistry(Box::new(CachePathInfoServiceConfig {
                near: "local".into(),
                far: "remote".into(),
            })),
        );
        comp.extend(path_info_configs);
    } else {
        comp.extend(std::iter::once((
            "default".into(),
            path_info_service_config,
        )));
    }

    let blob_service: Arc<dyn BlobService> = comp.build("default").await?;
    let directory_service: Arc<dyn DirectoryService> = comp.build("default").await?;
    let path_info_service: Arc<dyn PathInfoService> = comp.build("default").await?;

    // HACK: The grpc client also implements NarCalculationService, and we
    // really want to use it (otherwise we'd need to fetch everything again for hashing).
    // Until we revamped store composition and config, detect this special case here.
    let nar_calculation_service: Box<dyn NarCalculationService> = {
        use crate::pathinfoservice::GRPCPathInfoService;
        use crate::proto::path_info_service_client::PathInfoServiceClient;

        let url = Url::parse(path_info_service_addr.as_ref())
            .map_err(|e| io::Error::other(e.to_string()))?;

        if url.scheme().starts_with("grpc+") {
            Box::new(GRPCPathInfoService::from_client(
                PathInfoServiceClient::with_interceptor(
                    tvix_castore::tonic::channel_from_url(&url)
                        .await
                        .map_err(|e| io::Error::other(e.to_string()))?,
                    tvix_tracing::propagate::tonic::send_trace,
                ),
            ))
        } else {
            Box::new(SimpleRenderer::new(
                blob_service.clone(),
                directory_service.clone(),
            )) as Box<dyn NarCalculationService>
        }
    };

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
