use tonic::{Request, Response, Result, Status};

use crate::proto::blob_service_server::BlobService;
use crate::proto::{GetBlobRequest, GetBlobResponse, PutBlobRequest, PutBlobResponse};

#[derive(Debug, Default)]
pub struct NullBlobService {}

#[tonic::async_trait]
/// BlobService that always returns error
impl BlobService for NullBlobService {
    async fn get(&self, _request: Request<GetBlobRequest>) -> Result<Response<GetBlobResponse>> {
        Err(Status::unimplemented(""))
    }
    async fn put(&self, _request: Request<PutBlobRequest>) -> Result<Response<PutBlobResponse>> {
        Err(Status::unimplemented(""))
    }
}

#[cfg(test)]
mod tests {
    use crate::null_blob_service::NullBlobService;
    use crate::proto::blob_service_server::BlobService;
    use crate::proto::{GetBlobRequest, PutBlobRequest};

    #[tokio::test]
    async fn test_err() {
        let test_data = "Hello, World!".as_bytes().to_vec();
        let service = NullBlobService::default();

        let result = service
            .put(tonic::Request::new(PutBlobRequest {
                data: test_data.clone(),
            }))
            .await;

        assert!(result.is_err());
        if let Err(status) = result {
            assert_eq!(status.code(), tonic::Code::Unimplemented)
        } else {
            assert!(false)
        }

        let result = service
            .get(tonic::Request::new(GetBlobRequest {
                digest: blake3::hash(b"Hello").as_bytes().to_vec(),
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
