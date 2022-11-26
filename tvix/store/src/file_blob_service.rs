use std::io::Write;
use tonic::{Request, Response, Result, Status};
use tracing::{debug, error, info_span, instrument, warn};

use crate::proto::blob_service_server::BlobService;
use crate::proto::{GetBlobRequest, GetBlobResponse, PutBlobRequest, PutBlobResponse};

#[derive(Debug)]
pub struct FileBlobService {
    pub store_path: std::path::PathBuf,
}

#[tonic::async_trait]
/// BlobService that stores all blobs as files named by digest, in subdirectories for each possible first byte.
impl BlobService for FileBlobService {
    #[instrument(skip(self))]
    async fn get(&self, request: Request<GetBlobRequest>) -> Result<Response<GetBlobResponse>> {
        let digest = request.into_inner().digest;
        let _ = info_span!("digest", "{}", hex::encode(digest.clone())).enter();
        if digest.len() != 32 {
            let msg = format!(
                "Digest has wrong length {} but expected {}.)",
                digest.len(),
                32
            );
            warn!(msg);
            return Err(Status::invalid_argument(msg));
        }
        let (head, _) = digest.split_at(1);
        let dir_path = self.store_path.join(hex::encode(head));
        let blob_path = dir_path.join(hex::encode(digest.clone()));

        if blob_path.is_file() {
            match std::fs::read(blob_path) {
                Ok(data) => {
                    debug!("OK.");
                    Ok(tonic::Response::new(GetBlobResponse { data: data }))
                }
                Err(ref err) => {
                    let msg = "Error reading file.";
                    error!("{} {:?}", msg, err);
                    Err(Status::unknown(msg))
                }
            }
        } else {
            let msg = "Not found.";
            debug!(msg);
            Err(Status::not_found(msg))
        }
    }

    #[instrument(skip(self))]
    async fn put(&self, request: Request<PutBlobRequest>) -> Result<Response<PutBlobResponse>> {
        let data = &request.into_inner().data;
        let digest = blake3::hash(data).as_bytes().to_vec();
        let (head, _) = digest.split_at(1);
        let dir_path = self.store_path.join(hex::encode(head));
        let blob_path = dir_path.join(hex::encode(digest.clone()));
        let _ = info_span!("digest", "{}", hex::encode(digest.clone())).enter();

        if !dir_path.is_dir() {
            if let Err(err) = std::fs::create_dir_all(dir_path.clone()) {
                let msg = "Error creating directory.";
                error!("{} {:?}", msg, err);
                return Err(Status::internal(msg));
            }
        }
        if blob_path.exists() {
            debug!("blob already exists");
            return Ok(tonic::Response::new(PutBlobResponse { digest: digest }));
        }
        match tempfile::NamedTempFile::new_in(dir_path) {
            Err(err) => {
                let msg = "Error creating file.";
                error!("{} {:?}", msg, err);
                Err(Status::internal(msg))
            }
            Ok(file) => {
                if let Err(err) = file.as_file().write_all(data) {
                    let msg = "Error writing file.";
                    error!("{} {:?}", msg, err);
                    return Err(Status::internal(msg));
                }
                if let Err(err) = file.persist(blob_path) {
                    let msg = "Error renaming file.";
                    error!("{} {:?}", msg, err);
                    return Err(Status::internal(msg));
                }
                debug!("OK.");
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
