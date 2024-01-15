use crate::buildservice::BuildService;
use std::ops::Deref;
use tonic::async_trait;

use super::{Build, BuildRequest};

/// Implements the gRPC server trait ([self::build_service_server::BuildService]
/// for anything implementing [BuildService].
pub struct GRPCBuildServiceWrapper<BS> {
    inner: BS,
}

impl<BS> GRPCBuildServiceWrapper<BS> {
    pub fn new(build_service: BS) -> Self {
        Self {
            inner: build_service,
        }
    }
}

#[async_trait]
impl<BS> crate::proto::build_service_server::BuildService for GRPCBuildServiceWrapper<BS>
where
    BS: Deref<Target = dyn BuildService> + Send + Sync + 'static,
{
    async fn do_build(
        &self,
        request: tonic::Request<BuildRequest>,
    ) -> Result<tonic::Response<Build>, tonic::Status> {
        match self.inner.do_build(request.into_inner()).await {
            Ok(resp) => Ok(tonic::Response::new(resp)),
            Err(e) => Err(tonic::Status::internal(e.to_string())),
        }
    }
}
