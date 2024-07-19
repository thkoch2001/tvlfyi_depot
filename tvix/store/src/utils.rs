use std::sync::Arc;
use std::{
    pin::Pin,
    task::{self, Poll},
};
use tokio::io::{self, AsyncWrite};

use tvix_castore::{blobservice::BlobService, directoryservice::DirectoryService};
use url::Url;

use crate::composition::{Composition, DeserializeWithRegistry, ServiceBuilder};
use crate::nar::{NarCalculationService, SimpleRenderer};
use crate::pathinfoservice::PathInfoService;

/// Construct the store handles from their addrs.
pub async fn construct_services(
    blob_service_addr: impl AsRef<str>,
    directory_service_addr: impl AsRef<str>,
    path_info_service_addr: impl AsRef<str>,
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
    comp.extend(vec![(
        "default".into(),
        <DeserializeWithRegistry<Box<dyn ServiceBuilder<Output = dyn BlobService>>>>::try_from(
            Url::parse(blob_service_addr.as_ref())?,
        )?,
    )]);
    comp.extend(vec![(
        "default".into(),
        <DeserializeWithRegistry<Box<dyn ServiceBuilder<Output = dyn DirectoryService>>>>::try_from(Url::parse(directory_service_addr.as_ref())?)?
    )]);
    comp.extend(vec![(
        "default".into(),
        <DeserializeWithRegistry<Box<dyn ServiceBuilder<Output = dyn PathInfoService>>>>::try_from(
            Url::parse(path_info_service_addr.as_ref())?,
        )?,
    )]);
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
