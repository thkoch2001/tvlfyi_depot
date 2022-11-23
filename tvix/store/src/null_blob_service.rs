use tonic::{Request, Response, Result, Status};

pub mod proto {
    tonic::include_proto!("tvix.store.v1");
}

use crate::proto::blob_service_server::BlobService;
use crate::proto::{GetBlobRequest, GetBlobResponse, PutBlobRequest, PutBlobResponse};

#[derive(Debug, Default)]
pub struct NullBlobService {}

#[tonic::async_trait]
/// BlobService that always returns error
impl BlobService for NullBlobService {
    async fn get(&self, _request: Request<GetBlobRequest>) -> Result<Response<GetBlobResponse>> {
        Err(Status::unknown("error"))
    }
    async fn put(&self, _request: Request<PutBlobRequest>) -> Result<Response<PutBlobResponse>> {
        Err(Status::unknown("error"))
    }
}

// #[cfg(test)]
// mod tests {
//     use crate::proto::blob_service_server::BlobService;
//     use crate::proto::{GetBlobRequest, PutBlobRequest};
//     use crate::blob_service::FileBlobService;

//     #[tokio::test]
//     async fn test_get() {
//         let test_data = "Hello, World!".as_bytes().to_vec();
//         let service = NullBlobService::default();

//         let digest = service
//             .put(tonic::Request::new(PutBlobRequest {
//                 data: test_data.clone(),
//             }))
//             .await
//             .unwrap()
//             .into_inner()
//             .digest;
//         let data = service
//             .get(tonic::Request::new(GetBlobRequest { digest: digest }))
//             .await
//             .unwrap()
//             .into_inner()
//             .data;

//         assert_eq!(test_data, data);
//     }

//     #[tokio::test]
//     async fn test_write_write_read() {
//         // Put a blob and put it again. Should receive same digest. Then, get and check it.

//         let test_data = "Hello, World!".as_bytes().to_vec();

//         let service = FileBlobService {
//             store_path: std::path::Path::new("tvix-store-test"),
//         };
//         std::fs::remove_dir_all(service.store_path).ok();

//         let digest = service
//             .put(tonic::Request::new(PutBlobRequest {
//                 data: test_data.clone(),
//             }))
//             .await
//             .unwrap()
//             .into_inner()
//             .digest;
//         let digest2 = service
//             .put(tonic::Request::new(PutBlobRequest {
//                 data: test_data.clone(),
//             }))
//             .await
//             .unwrap()
//             .into_inner()
//             .digest;
//         assert_eq!(digest, digest2);
//         let data = service
//             .get(tonic::Request::new(GetBlobRequest { digest: digest }))
//             .await
//             .unwrap()
//             .into_inner()
//             .data;

//         assert_eq!(test_data, data);
//     }

//     #[tokio::test]
//     async fn test_get_nonexistent() {
//         // Try to get a nonexistent blob

//         let service = FileBlobService {
//             store_path: std::path::Path::new("tvix-store-test"),
//         };
//         std::fs::remove_dir_all(service.store_path).ok();

//         let digest = blake3::hash("the contents of a blob not there".as_bytes()).as_bytes().to_vec();
//         let result = service
//             .get(tonic::Request::new(GetBlobRequest { digest: digest }))
//             .await;
//         assert!(result.is_err());

//         match result {
//             Err(status) =>
//                 status
//         }

//         }
// }
