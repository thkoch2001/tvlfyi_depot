use futures::StreamExt;
use sled::transaction::{abort, TransactionError};
use std::path::PathBuf;
use tokio::sync::mpsc;
use tokio_stream::wrappers::ReceiverStream;
use tonic::{Request, Response, Result, Status, Streaming};
use tracing::{debug, error, info_span, instrument, warn};

use crate::proto::blob_service_server::BlobService;
use crate::proto::{
    blob_meta::ChunkMeta, BlobChunk, BlobMeta, PutBlobResponse, ReadBlobRequest, StatBlobRequest,
};

pub struct SledBlobService {
    db: sled::Db,
}

impl SledBlobService {
    pub fn new(p: PathBuf) -> Result<Self, anyhow::Error> {
        let config = sled::Config::default()
            .use_compression(true)
            .mode(sled::Mode::LowSpace)
            .path(p);
        let db = config.open()?;

        Ok(Self { db })
    }
}

#[tonic::async_trait]
/// BlobService that stores all blobs as individual objects in sled, keyed by their blake3 hash.
// TODO: we need to store smaller chunks here, ideally using a CDC to persist stuff.
// TODO: our intermediate chunking info struct should probably be a proto too.
impl BlobService for SledBlobService {
    type ReadStream = ReceiverStream<Result<BlobChunk>>;

    #[instrument(skip(self))]
    async fn read(
        &self,
        request: Request<ReadBlobRequest>,
    ) -> Result<Response<Self::ReadStream>, Status> {
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

        let (tx, rx) = mpsc::channel::<Result<BlobChunk>>(10);

        // Spawn a (blocking) thread that'll query the blobs bucket.
        let db = self.db.clone();

        tokio::task::spawn_blocking(move || {
            let resp = db.transaction::<_, (), Status>(|db_txn| match db_txn.get(&digest)? {
                Some(v) => match tx.blocking_send(Ok(BlobChunk { data: v.to_vec() })) {
                    Ok(()) => Ok(()),
                    Err(e) => {
                        warn!("failed to send blob chunk: {e}");
                        abort(Status::internal("failed to send blob chunk"))
                    }
                },
                None => {
                    warn!("blob not found");
                    abort(Status::not_found("blob not found"))
                }
            });

            match resp {
                Ok(()) => Ok(()),
                Err(TransactionError::Abort(status)) => tx.blocking_send(Err(status)),
                Err(TransactionError::Storage(e)) => {
                    error!("storage error: {:?}", e);
                    tx.blocking_send(Err(Status::internal("storage error")))
                }
            }
            .unwrap_or_else(|e| {
                warn!("failed to send error to client: {e}");
            });
        });

        let receiver_stream = ReceiverStream::new(rx);
        Ok(Response::new(receiver_stream))
    }

    #[instrument(skip(self))]
    async fn stat(&self, request: Request<StatBlobRequest>) -> Result<Response<BlobMeta>> {
        Ok(Response::new(BlobMeta {
            chunks: vec![ChunkMeta {
                digest: request.into_inner().digest,
                size: 0x42, // TODO: implement
            }],
            inline_bao: vec![],
        }))
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
                Some(Ok(mut chunk)) => {
                    received_data.append(&mut chunk.data);
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

        let _ = info_span!("digest", "{}", base64::encode(&digest));

        // TODO: pipe data through a chunker, and store individual chunks
        // build up a chunk meta and on EOF store this to sled as well.

        // TODO: can there be no failures here?
        let dgst = digest.clone();
        let resp = self.db.transaction::<_, (), Status>(move |db_txn| {
            if db_txn.get(&dgst)?.is_none() {
                db_txn.insert(&*dgst, &*received_data)?;
                Ok(())
            } else {
                debug!("digest already exists");
                Ok(())
            }
        });

        match resp {
            Ok(()) => Ok(Response::new(PutBlobResponse { digest })),
            Err(TransactionError::Abort(status)) => Err(status),
            Err(TransactionError::Storage(e)) => {
                error!("storage error: {:?}", e);
                Err(Status::internal("storage error"))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use tokio_stream::StreamExt;

    use crate::proto::blob_service_server::BlobService;
    use crate::proto::{BlobChunk, ReadBlobRequest};
    use crate::sled_blob_service::SledBlobService;

    #[tokio::test]
    /// Put a blob and get it by the returned digest.
    async fn test_write_read() -> anyhow::Result<()> {
        let test_data = "Hello, World!".as_bytes().to_vec();

        let store_path = std::path::PathBuf::from("tvix-store-test_write_read");
        std::fs::remove_dir_all(&store_path).ok();
        let service = SledBlobService::new(store_path.clone())?;

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
            .read(tonic::Request::new(ReadBlobRequest {
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
        std::fs::remove_dir_all(store_path.clone()).ok();
        Ok(())
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
    //         .get(tonic::Request::new(ReadBlobRequest { digest: digest }))
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
    //         .get(tonic::Request::new(ReadBlobRequest {
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
