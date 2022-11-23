use std::io::Write;

use tonic::{Request, Response, Result, Status};

use crate::proto::blob_service_server::BlobService;
use crate::proto::{GetBlobRequest, GetBlobResponse, PutBlobRequest, PutBlobResponse};

#[derive(Debug)]
pub struct FileBlobService {
    pub store_path: std::path::PathBuf,
}

#[tonic::async_trait]
/// BlobService that stores all blobs as files named by digest, in subdirectories for each possible first byte.
impl BlobService for FileBlobService {
    async fn get(&self, request: Request<GetBlobRequest>) -> Result<Response<GetBlobResponse>> {
        let digest = request.into_inner().digest;
        if digest.len() != 32 {
            return Err(Status::unknown(format!(
                "Digest has wrong length: {}",
                digest.len()
            )));
        }
        let (head, _) = digest.split_at(1);
        let dir_path = self.store_path.join(hex::encode(head));
        let blob_path = dir_path.join(hex::encode(digest.clone()));

        if blob_path.is_file() {
            if let Ok(data) = std::fs::read(blob_path) {
                Ok(tonic::Response::new(GetBlobResponse { data: data }))
            } else {
                Err(Status::unknown("Error reading file."))
            }
        } else {
            Err(Status::not_found("Not found."))
        }
    }

    async fn put(&self, request: Request<PutBlobRequest>) -> Result<Response<PutBlobResponse>> {
        let data = &request.into_inner().data;
        let digest = blake3::hash(data).as_bytes().to_vec();
        let (head, _) = digest.split_at(1);
        let dir_path = self.store_path.join(hex::encode(head));
        let blob_path = dir_path.join(hex::encode(digest.clone()));

        if !dir_path.is_dir() {
            if let Err(_) = std::fs::create_dir_all(dir_path.clone()) {
                return Err(Status::unknown("Error creating directory."));
            }
        }
        if blob_path.exists() {
            return Ok(tonic::Response::new(PutBlobResponse { digest: digest }));
        }
        match tempfile::NamedTempFile::new_in(dir_path) {
            Err(err) => Err(Status::unknown(format!("Error creating file: ?{}", err))),
            Ok(file) => {
                if let Err(err) = file.as_file().write_all(data) {
                    return Err(Status::unknown(format!("Error writing file: ?{}", err)));
                }
                if let Err(err) = file.persist(blob_path) {
                    return Err(Status::unknown(format!("Error renaming file: ?{}", err)));
                }
                Ok(tonic::Response::new(PutBlobResponse { digest: digest }))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::file_blob_service::FileBlobService;
    use crate::proto::blob_service_server::BlobService;
    use crate::proto::{GetBlobRequest, PutBlobRequest};

    #[tokio::test]
    /// Put a blob and get it by the returned digest.
    async fn test_write_read() {
        let test_data = "Hello, World!".as_bytes().to_vec();

        let service = FileBlobService {
            store_path: std::path::PathBuf::from("tvix-store-test_write_read"),
        };
        std::fs::remove_dir_all(service.store_path.clone()).ok();

        let digest = service
            .put(tonic::Request::new(PutBlobRequest {
                data: test_data.clone(),
            }))
            .await
            .unwrap()
            .into_inner()
            .digest;
        let data = service
            .get(tonic::Request::new(GetBlobRequest { digest: digest }))
            .await
            .unwrap()
            .into_inner()
            .data;

        assert_eq!(test_data, data);
        std::fs::remove_dir_all(service.store_path.clone()).ok();
    }

    #[tokio::test]
    /// Put a blob and put it again. Should receive same digest. Then, get and check it.
    async fn test_write_write_read() {
        let test_data = "Hello, World!".as_bytes().to_vec();

        let service = FileBlobService {
            store_path: std::path::PathBuf::from("tvix-store-test_write_write_read"),
        };
        std::fs::remove_dir_all(service.store_path.clone()).ok();

        let digest = service
            .put(tonic::Request::new(PutBlobRequest {
                data: test_data.clone(),
            }))
            .await
            .unwrap()
            .into_inner()
            .digest;
        let digest2 = service
            .put(tonic::Request::new(PutBlobRequest {
                data: test_data.clone(),
            }))
            .await
            .unwrap()
            .into_inner()
            .digest;
        assert_eq!(digest, digest2);
        let data = service
            .get(tonic::Request::new(GetBlobRequest { digest: digest }))
            .await
            .unwrap()
            .into_inner()
            .data;

        assert_eq!(test_data, data);
        std::fs::remove_dir_all(service.store_path.clone()).ok();
    }

    #[tokio::test]
    /// Try to get a nonexistent blob
    async fn test_get_nonexistent() {
        let service = FileBlobService {
            store_path: std::path::PathBuf::from("tvix-store-test_get_nonexistent"),
        };
        std::fs::remove_dir_all(service.store_path.clone()).ok();

        let digest = blake3::hash("the contents of a blob not there".as_bytes())
            .as_bytes()
            .to_vec();
        let result = service
            .get(tonic::Request::new(GetBlobRequest {
                digest: digest.clone(),
            }))
            .await;
        assert!(result.is_err());

        if let Err(status) = result {
            assert_eq!(status.code(), tonic::Code::NotFound)
        } else {
            assert!(false)
        }
        std::fs::remove_dir_all(service.store_path.clone()).ok();
    }
}
