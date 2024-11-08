use super::{PathInfo, PathInfoService};
use crate::{
    nar::NarCalculationService,
    proto::{self, ListPathInfoRequest},
};
use async_stream::try_stream;
use futures::stream::BoxStream;
use nix_compat::nixbase32;
use std::sync::Arc;
use tonic::{async_trait, Code};
use tracing::{instrument, warn, Span};
use tracing_indicatif::span_ext::IndicatifSpanExt;
use tvix_castore::composition::{CompositionContext, ServiceBuilder};
use tvix_castore::Error;
use tvix_castore::Node;

/// Connects to a (remote) tvix-store PathInfoService over gRPC.
#[derive(Clone)]
pub struct GRPCPathInfoService<T> {
    instance_name: String,

    /// The internal reference to a gRPC client.
    /// Cloning it is cheap, and it internally handles concurrent requests.
    grpc_client: proto::path_info_service_client::PathInfoServiceClient<T>,
}

impl<T> GRPCPathInfoService<T> {
    /// construct a [GRPCPathInfoService] from a [proto::path_info_service_client::PathInfoServiceClient].
    /// panics if called outside the context of a tokio runtime.
    pub fn from_client(
        instance_name: String,
        grpc_client: proto::path_info_service_client::PathInfoServiceClient<T>,
    ) -> Self {
        Self {
            instance_name,
            grpc_client,
        }
    }
}

#[async_trait]
impl<T> PathInfoService for GRPCPathInfoService<T>
where
    T: tonic::client::GrpcService<tonic::body::BoxBody> + Send + Sync + Clone + 'static,
    T::ResponseBody: tonic::codegen::Body<Data = tonic::codegen::Bytes> + Send + 'static,
    <T::ResponseBody as tonic::codegen::Body>::Error: Into<tonic::codegen::StdError> + Send,
    T::Future: Send,
{
    #[instrument(level = "trace", skip_all, fields(path_info.digest = nixbase32::encode(&digest), instance_name = %self.instance_name))]
    async fn get(&self, digest: [u8; 20]) -> Result<Option<PathInfo>, Error> {
        let path_info = self
            .grpc_client
            .clone()
            .get(proto::GetPathInfoRequest {
                by_what: Some(proto::get_path_info_request::ByWhat::ByOutputHash(
                    digest.to_vec().into(),
                )),
            })
            .await;

        match path_info {
            Ok(path_info) => Ok(Some(
                PathInfo::try_from(path_info.into_inner())
                    .map_err(|e| Error::StorageError(format!("Invalid path info: {e}")))?,
            )),
            Err(e) if e.code() == Code::NotFound => Ok(None),
            Err(e) => Err(Error::StorageError(e.to_string())),
        }
    }

    #[instrument(level = "trace", skip_all, fields(path_info.root_node = ?path_info.node, instance_name = %self.instance_name))]
    async fn put(&self, path_info: PathInfo) -> Result<PathInfo, Error> {
        let path_info = self
            .grpc_client
            .clone()
            .put(proto::PathInfo::from(path_info))
            .await
            .map_err(|e| Error::StorageError(e.to_string()))?
            .into_inner();
        Ok(PathInfo::try_from(path_info)
            .map_err(|e| Error::StorageError(format!("Invalid path info: {e}")))?)
    }

    #[instrument(level = "trace", skip_all)]
    fn list(&self) -> BoxStream<'static, Result<PathInfo, Error>> {
        let mut grpc_client = self.grpc_client.clone();

        let stream = try_stream! {
            let resp = grpc_client.list(ListPathInfoRequest::default()).await;

            let mut stream = resp.map_err(|e| Error::StorageError(e.to_string()))?.into_inner();

            loop {
                match stream.message().await {
                    Ok(Some(path_info)) => yield PathInfo::try_from(path_info).map_err(|e| Error::StorageError(format!("Invalid path info: {e}")))?,
                    Ok(None) => return,
                    Err(e) => Err(Error::StorageError(e.to_string()))?,
                }
            }
        };

        Box::pin(stream)
    }

    #[instrument(level = "trace", skip_all)]
    fn nar_calculation_service(&self) -> Option<Box<dyn NarCalculationService>> {
        Some(Box::new(GRPCPathInfoService {
            instance_name: self.instance_name.clone(),
            grpc_client: self.grpc_client.clone(),
        }) as Box<dyn NarCalculationService>)
    }
}

#[async_trait]
impl<T> NarCalculationService for GRPCPathInfoService<T>
where
    T: tonic::client::GrpcService<tonic::body::BoxBody> + Send + Sync + Clone + 'static,
    T::ResponseBody: tonic::codegen::Body<Data = tonic::codegen::Bytes> + Send + 'static,
    <T::ResponseBody as tonic::codegen::Body>::Error: Into<tonic::codegen::StdError> + Send,
    T::Future: Send,
{
    #[instrument(level = "trace", skip_all, fields(root_node = ?root_node, indicatif.pb_show=1))]
    async fn calculate_nar(&self, root_node: &Node) -> Result<(u64, [u8; 32]), Error> {
        let span = Span::current();
        span.pb_set_message("Waiting for NAR calculation");
        span.pb_start();

        let path_info = self
            .grpc_client
            .clone()
            .calculate_nar(tvix_castore::proto::Node::from_name_and_node(
                "".into(),
                root_node.to_owned(),
            ))
            .await
            .map_err(|e| Error::StorageError(e.to_string()))?
            .into_inner();

        let nar_sha256: [u8; 32] = path_info
            .nar_sha256
            .to_vec()
            .try_into()
            .map_err(|_e| Error::StorageError("invalid digest length".to_string()))?;

        Ok((path_info.nar_size, nar_sha256))
    }
}

#[derive(serde::Deserialize, Debug)]
#[serde(deny_unknown_fields)]
pub struct GRPCPathInfoServiceConfig {
    url: String,
}

impl TryFrom<url::Url> for GRPCPathInfoServiceConfig {
    type Error = Box<dyn std::error::Error + Send + Sync>;
    fn try_from(url: url::Url) -> Result<Self, Self::Error> {
        //   normally grpc+unix for unix sockets, and grpc+http(s) for the HTTP counterparts.
        // - In the case of unix sockets, there must be a path, but may not be a host.
        // - In the case of non-unix sockets, there must be a host, but no path.
        // Constructing the channel is handled by tvix_castore::channel::from_url.
        Ok(GRPCPathInfoServiceConfig {
            url: url.to_string(),
        })
    }
}

#[async_trait]
impl ServiceBuilder for GRPCPathInfoServiceConfig {
    type Output = dyn PathInfoService;
    async fn build<'a>(
        &'a self,
        instance_name: &str,
        _context: &CompositionContext,
    ) -> Result<Arc<dyn PathInfoService>, Box<dyn std::error::Error + Send + Sync + 'static>> {
        let client = proto::path_info_service_client::PathInfoServiceClient::new(
            tvix_castore::tonic::channel_from_url(&self.url.parse()?).await?,
        );
        Ok(Arc::new(GRPCPathInfoService::from_client(
            instance_name.to_string(),
            client,
        )))
    }
}

#[cfg(test)]
mod tests {
    use crate::pathinfoservice::tests::make_grpc_path_info_service_client;
    use crate::pathinfoservice::PathInfoService;
    use crate::tests::fixtures;

    /// This ensures connecting via gRPC works as expected.
    #[tokio::test]
    async fn test_valid_unix_path_ping_pong() {
        let (_blob_service, _directory_service, path_info_service) =
            make_grpc_path_info_service_client().await;

        let path_info = path_info_service
            .get(fixtures::DUMMY_PATH_DIGEST)
            .await
            .expect("must not be error");

        assert!(path_info.is_none());
    }
}
