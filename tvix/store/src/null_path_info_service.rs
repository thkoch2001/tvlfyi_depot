use tonic::{Request, Response, Result, Status};

use crate::proto::path_info_service_server::PathInfoService;
use crate::proto::{GetPathInfoRequest, PathInfo, PutPathInfoRequest};

#[derive(Debug, Default)]
pub struct NullPathInfoService {}

#[tonic::async_trait]
/// PathinfoService that always returns error
impl PathInfoService for NullPathInfoService {
    async fn get(&self, _request: Request<GetPathInfoRequest>) -> Result<Response<PathInfo>> {
        Err(Status::unimplemented(""))
    }
    async fn put(&self, _request: Request<PutPathInfoRequest>) -> Result<Response<PathInfo>> {
        Err(Status::unimplemented(""))
    }
}

#[cfg(test)]
mod tests {
    use crate::null_path_info_service::NullPathInfoService;
    use crate::proto::path_info_service_server::PathInfoService;
    use crate::proto::{GetPathInfoRequest, PathInfo, PutPathInfoRequest};

    #[tokio::test]
    async fn test_err() {
        let service = NullPathInfoService::default();

        let result = service
            .put(tonic::Request::new(PutPathInfoRequest {
                path_info: Some(PathInfo {
                    references: vec![],
                    narinfo: None,
                    node: None,
                }),
                output_hash: vec![],
            }))
            .await;
        assert!(result.is_err());
        if let Err(status) = result {
            assert_eq!(status.code(), tonic::Code::Unimplemented)
        } else {
            assert!(false)
        }

        let result = service
            .get(tonic::Request::new(GetPathInfoRequest { by_what: None }))
            .await;
        assert!(result.is_err());
        if let Err(status) = result {
            assert_eq!(status.code(), tonic::Code::Unimplemented)
        } else {
            assert!(false)
        }
    }
}
