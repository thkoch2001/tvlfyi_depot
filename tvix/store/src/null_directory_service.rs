use crate::proto::directory_service_server::DirectoryService;
use crate::proto::{Directory, GetDirectoryRequest, PutDirectoryResponse};
use futures::Stream;
use std::pin::Pin;
use tonic::{Request, Response, Result, Status, Streaming};

#[derive(Debug, Default)]
pub struct NullDirectoryService {}

#[tonic::async_trait]
/// DirectoryService that always returns error
impl DirectoryService for NullDirectoryService {
    type GetStream = Pin<Box<dyn Stream<Item = Result<Directory, Status>> + Send>>;

    async fn get(
        &self,
        _request: Request<GetDirectoryRequest>,
    ) -> Result<Response<Self::GetStream>, Status> {
        Err(Status::unimplemented(""))
    }

    async fn put(
        &self,
        _request: Request<Streaming<Directory>>,
    ) -> Result<Response<PutDirectoryResponse>, Status> {
        Err(Status::unimplemented(""))
    }
}

#[cfg(test)]
mod tests {
    use crate::null_directory_service::NullDirectoryService;
    use crate::proto::directory_service_server::DirectoryService;
    use crate::proto::GetDirectoryRequest;

    #[tokio::test]
    async fn test_err() {
        let service = NullDirectoryService::default();

        // TODO: test get

        let result = service
            .get(tonic::Request::new(GetDirectoryRequest {
                by_what: None,
                recursive: false,
            }))
            .await;
        assert!(result.is_err());
        if let Err(status) = result {
            assert_eq!(status.code(), tonic::Code::Unimplemented)
        } else {
            assert!(false)
        }
    }
}
