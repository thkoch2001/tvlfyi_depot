use crate::nar::RenderError;
use crate::pathinfoservice::PathInfoService;
use crate::proto;
use futures::{stream::BoxStream, TryStreamExt};
use proto::calculate_digest_request::Source;
use std::ops::Deref;
use tonic::{async_trait, Request, Response, Result, Status};
use tracing::{instrument, warn};

use super::nar_info;

pub struct GRPCPathInfoServiceWrapper<PS> {
    inner: PS,
    // FUTUREWORK: allow exposing without allowing listing
}

impl<PS> GRPCPathInfoServiceWrapper<PS> {
    pub fn new(path_info_service: PS) -> Self {
        Self {
            inner: path_info_service,
        }
    }
}

#[async_trait]
impl<PS> proto::path_info_service_server::PathInfoService for GRPCPathInfoServiceWrapper<PS>
where
    PS: Deref<Target = dyn PathInfoService> + Send + Sync + 'static,
{
    type ListStream = BoxStream<'static, tonic::Result<proto::PathInfo, Status>>;

    #[instrument(skip(self))]
    async fn get(
        &self,
        request: Request<proto::GetPathInfoRequest>,
    ) -> Result<Response<proto::PathInfo>> {
        match request.into_inner().by_what {
            None => Err(Status::unimplemented("by_what needs to be specified")),
            Some(proto::get_path_info_request::ByWhat::ByOutputHash(output_digest)) => {
                let digest: [u8; 20] = output_digest
                    .to_vec()
                    .try_into()
                    .map_err(|_e| Status::invalid_argument("invalid output digest length"))?;
                match self.inner.get(digest).await {
                    Ok(None) => Err(Status::not_found("PathInfo not found")),
                    Ok(Some(path_info)) => Ok(Response::new(path_info)),
                    Err(e) => {
                        warn!("failed to retrieve PathInfo: {}", e);
                        Err(e.into())
                    }
                }
            }
        }
    }

    #[instrument(skip(self))]
    async fn put(&self, request: Request<proto::PathInfo>) -> Result<Response<proto::PathInfo>> {
        let path_info = request.into_inner();

        // Store the PathInfo in the client. Clients MUST validate the data
        // they receive, so we don't validate additionally here.
        match self.inner.put(path_info).await {
            Ok(path_info_new) => Ok(Response::new(path_info_new)),
            Err(e) => {
                warn!("failed to insert PathInfo: {}", e);
                Err(e.into())
            }
        }
    }

    #[instrument(skip(self))]
    async fn calculate_digest(
        &self,
        request: Request<proto::CalculateDigestRequest>,
    ) -> Result<Response<proto::CalculateDigestResponse>> {
        let r = request.into_inner();
        // TODO: move this into CalculateDigestRequest::validate()
        let source = r.source.unwrap();
        match r.r#type {
            typ if typ == nar_info::ca::Hash::NarSha256 as i32 => match source {
                Source::RootNode(root_node) => {
                    let (nar_size, nar_sha256) = self
                        .inner
                        .calculate_nar(&root_node.node.unwrap()) // TODO: have CalculateDigestRequest::validate() catch this
                        .await
                        .expect("error during nar calculation"); // TODO: handle error

                    Ok(Response::new(proto::CalculateDigestResponse {
                        nar_size,
                        digest: nar_sha256.to_vec().into(),
                    }))
                }
                // TODO: move this into CalculateDigestRequest::validate()
                Source::BlobDigest(_) => Err(tonic::Status::unimplemented(
                    "blob digest must not be set for NAR",
                ))?,
            },
            typ if (typ == nar_info::ca::Hash::NarSha1 as i32
                || typ == nar_info::ca::Hash::NarSha256 as i32
                || typ == nar_info::ca::Hash::NarMd5 as i32) =>
            {
                Err(tonic::Status::unimplemented(
                    "only NAR_SHA implemented currently",
                ))?
            }
            _ => Err(tonic::Status::unimplemented("hashing type not supported"))?,
        }
    }

    #[instrument(skip(self))]
    async fn list(
        &self,
        _request: Request<proto::ListPathInfoRequest>,
    ) -> Result<Response<Self::ListStream>, Status> {
        let stream = Box::pin(
            self.inner
                .list()
                .map_err(|e| Status::internal(e.to_string())),
        );

        Ok(Response::new(Box::pin(stream)))
    }
}

impl From<RenderError> for tonic::Status {
    fn from(value: RenderError) -> Self {
        match value {
            RenderError::BlobNotFound(_, _) => Self::not_found(value.to_string()),
            RenderError::DirectoryNotFound(_, _) => Self::not_found(value.to_string()),
            RenderError::NARWriterError(_) => Self::internal(value.to_string()),
            RenderError::StoreError(_) => Self::internal(value.to_string()),
            RenderError::UnexpectedBlobMeta(_, _, _, _) => Self::internal(value.to_string()),
        }
    }
}
