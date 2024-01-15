use tonic::{async_trait, transport::Channel};

use crate::proto::{build_service_client::BuildServiceClient, Build, BuildRequest};

use super::BuildService;

pub struct GRPCBuildService {
    client: BuildServiceClient<Channel>,
}

impl GRPCBuildService {
    pub fn from_client(client: BuildServiceClient<Channel>) -> Self {
        Self { client }
    }
}

#[async_trait]
impl BuildService for GRPCBuildService {
    async fn do_build(&self, request: BuildRequest) -> std::io::Result<Build> {
        let mut client = self.client.clone();
        client
            .do_build(request)
            .await
            .map(|resp| resp.into_inner())
            .map_err(|e| std::io::Error::other(e))
    }
}
