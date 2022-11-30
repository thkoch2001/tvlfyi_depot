use futures::StreamExt;
use std::io::Write;
use tokio::sync::mpsc;
use tokio_stream::wrappers::ReceiverStream;
use tonic::{Request, Response, Result, Status, Streaming};
use tracing::{debug, error, info_span, instrument, warn};

use crate::proto::blob_service_server::BlobService;
use crate::proto::{BlobChunk, GetBlobRequest, PutBlobResponse};

#[derive(Debug)]
pub struct FileBlobService {
    pub store_path: std::path::PathBuf,
}

#[tonic::async_trait]
/// BlobService that stores all blobs as files named by digest, in subdirectories for each possible first byte.
// We currently send one big chunk for the whole file.
impl BlobService for FileBlobService {
    type GetStream = ReceiverStream<Result<BlobChunk>>;

    #[instrument(skip(self))]
    async fn get(
        &self,
        request: Request<GetBlobRequest>,
    ) -> Result<Response<Self::GetStream>, Status> {
        let digest = request.into_inner().digest;
        let _ = info_span!("digest", "{}", base64::encode(digest.clone())).enter();
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

                    let (tx, rx) = mpsc::channel(10);
                    tokio::spawn(async move {
                        tx.send(Ok(BlobChunk { data })).await.ok();
                    });
                    let receiver_stream = ReceiverStream::new(rx);
                    Ok(Response::new(receiver_stream))
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

    #[instrument(skip(self, request))]
    async fn put(
        &self,
        request: Request<Streaming<BlobChunk>>,
    ) -> Result<Response<PutBlobResponse>> {
        let mut chunk_stream = request.into_inner();

        let mut received_data: Vec<u8> = Vec::default();

        // Receive all chunks one by one into received_data
        loop {
            match chunk_stream.next().await {
                Some(Ok(chunk)) => {
                    received_data.append(&mut chunk.data.clone());
                }
                Some(Err(err)) => {
                    warn!(?err, "Received error in chunk stream.");
                    return Err(err);
                }
                // end of the stream
                None => {
                    break;
                }
            }
        }

        let digest = blake3::hash(&received_data).as_bytes().to_vec();
        let (head, _) = digest.split_at(1);
        let dir_path = self.store_path.join(hex::encode(head));
        let blob_path = dir_path.join(hex::encode(digest.clone()));
        let _ = info_span!("digest", "{}", base64::encode(digest.clone())).enter();

        if !dir_path.is_dir() {
            if let Err(err) = std::fs::create_dir_all(dir_path.clone()) {
                let msg = "Error creating directory.";
                error!("{} {:?}", msg, err);
                return Err(Status::internal(msg));
            }
        }
        if blob_path.exists() {
            debug!("blob already exists");
            return Ok(tonic::Response::new(PutBlobResponse { digest }));
        }
        match tempfile::NamedTempFile::new_in(dir_path) {
            Err(err) => {
                let msg = "Error creating file.";
                error!(?err, "{}", msg);
                Err(Status::internal(msg))
            }
            Ok(file) => {
                if let Err(err) = file.as_file().write_all(&received_data) {
                    let msg = "Error writing file.";
                    error!(?err, "{}", msg);
                    return Err(Status::internal(msg));
                }
                if let Err(err) = file.persist(blob_path) {
                    let msg = "Error renaming file.";
                    error!(?err, "{}", msg);
                    return Err(Status::internal(msg));
                }
                debug!("OK.");
                Ok(tonic::Response::new(PutBlobResponse { digest }))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use tokio_stream::StreamExt;

    use crate::file_blob_service::FileBlobService;
    use crate::proto::blob_service_server::BlobService;
    use crate::proto::{BlobChunk, GetBlobRequest};

    #[tokio::test]
    /// Put a blob and get it by the returned digest.
    async fn test_write_read() {
        let test_data = "Hello, World!".as_bytes().to_vec();

        let service = FileBlobService {
            store_path: std::path::PathBuf::from("tvix-store-test_write_read"),
        };
        std::fs::remove_dir_all(service.store_path.clone()).ok();

        let streaming_request = tonic_mock::streaming_request(vec![
            BlobChunk {
                data: test_data[0..5].to_vec(),
            },
            BlobChunk {
                data: test_data[5..].to_vec(),
            },
        ]);

        let put_response = service.put(streaming_request).await.unwrap().into_inner();
        assert_eq!(put_response.digest.len(), 32);

        let mut get_stream = service
            .get(tonic::Request::new(GetBlobRequest {
                digest: put_response.digest,
            }))
            .await
            .unwrap()
            .into_inner();

        let mut blob_contents = Vec::new();
        while let Some(v) = get_stream.next().await {
            blob_contents.append(&mut v.unwrap().data.clone())
        }

        assert_eq!(blob_contents.len(), test_data.len());

        assert_eq!(blob_contents, test_data);
        std::fs::remove_dir_all(service.store_path.clone()).ok();
    }

    // #[tokio::test]
    // /// Put a blob and put it again. Should receive same digest. Then, get and check it.
    // async fn test_write_write_read() {
    //     let test_data = "Hello, World!".as_bytes().to_vec();

    //     let service = FileBlobService {
    //         store_path: std::path::PathBuf::from("tvix-store-test_write_write_read"),
    //     };
    //     std::fs::remove_dir_all(service.store_path.clone()).ok();

    //     let digest = service
    //         .put(tonic::Request::new(BlobChunk {
    //             data: test_data.clone(),
    //         }))
    //         .await
    //         .unwrap()
    //         .into_inner()
    //         .digest;
    //     let digest2 = service
    //         .put(tonic::Request::new(BlobChunk {
    //             data: test_data.clone(),
    //         }))
    //         .await
    //         .unwrap()
    //         .into_inner()
    //         .digest;
    //     assert_eq!(digest, digest2);
    //     let data = service
    //         .get(tonic::Request::new(GetBlobRequest { digest: digest }))
    //         .await
    //         .unwrap()
    //         .into_inner()
    //         .data;

    //     assert_eq!(test_data, data);
    //     std::fs::remove_dir_all(service.store_path.clone()).ok();
    // }

    // #[tokio::test]
    // /// Try to get a nonexistent blob
    // async fn test_get_nonexistent() {
    //     let service = FileBlobService {
    //         store_path: std::path::PathBuf::from("tvix-store-test_get_nonexistent"),
    //     };
    //     std::fs::remove_dir_all(service.store_path.clone()).ok();

    //     let digest = blake3::hash("the contents of a blob not there".as_bytes())
    //         .as_bytes()
    //         .to_vec();
    //     let result = service
    //         .get(tonic::Request::new(GetBlobRequest {
    //             digest: digest.clone(),
    //         }))
    //         .await;
    //     assert!(result.is_err());

    //     if let Err(status) = result {
    //         assert_eq!(status.code(), tonic::Code::NotFound)
    //     } else {
    //         assert!(false)
    //     }
    //     std::fs::remove_dir_all(service.store_path.clone()).ok();
    // }
}
