use std::path::PathBuf;

use prost::Message;
use tokio::sync::mpsc;
use tokio_stream::wrappers::ReceiverStream;

use crate::proto::directory_service_server::DirectoryService;
use crate::proto::get_directory_request::ByWhat;
use crate::proto::Directory;
use crate::proto::GetDirectoryRequest;
use crate::proto::PutDirectoryResponse;
use tonic::{Request, Response, Result, Status, Streaming};
use tracing::{debug, instrument, warn};

const NOT_IMPLEMENTED_MSG: &str = "not implemented";

pub struct SledDirectoryService {
    db: sled::Db,
}

impl SledDirectoryService {
    pub fn new(p: PathBuf) -> Result<Self, anyhow::Error> {
        let config = sled::Config::default().use_compression(true).path(p);
        let db = config.open()?;

        Ok(Self { db })
    }
}

#[tonic::async_trait]
impl DirectoryService for SledDirectoryService {
    type GetStream = ReceiverStream<Result<Directory>>;

    #[instrument(skip(self))]
    async fn get(
        &self,
        request: Request<GetDirectoryRequest>,
    ) -> Result<Response<Self::GetStream>, Status> {
        match request.into_inner().by_what {
            None => Err(Status::unimplemented("by_what needs to be specified")),
            Some(ByWhat::Digest(digest)) => {
                if digest.len() != 32 {
                    return Err(Status::invalid_argument("invalid digest length"));
                }
                // TODO: currently, recursive queries are not supported
                if request.into_inner().recursive {
                    return Err(Status::unimplemented("recursive queries are not supported"));
                }

                // TODO: when do we want to spawn? It's probably hard to move the txn between threads…

                let (tx, rx) = mpsc::channel(10);

                tokio::spawn(async move {
                    let txn_res = self.db.transaction(|txn_db| {
                        match txn_db.get(digest) {
                            // The directory was not found, abort
                            Ok(None) => {
                                Err(sled::transaction::ConflictableTransactionError::Abort(
                                    Status::not_found(format!(
                                        "directory {} not found",
                                        base64::encode(digest)
                                    )),
                                ))
                            }
                            // The directory was found
                            Ok(Some(data)) => {
                                // try to parse the data as Directory message
                                match Directory::decode(&*data) {
                                    Ok(directory) => {
                                        // TODO: validate the checksum matches to rule out storage errors?
                                        // send the directory message to the client
                                        tx.send(Ok(directory)).await;
                                        // TODO: catch error during sending?

                                        // FUTUREWORK: recursion
                                        Ok(())
                                    }
                                }
                            }
                            Err(e) => {
                                warn!("failed to retrieve directory: {}", e);
                                Err(sled::transaction::ConflictableTransactionError::Abort(
                                    Status::internal("error during directory lookup"),
                                ))
                            }
                        }
                        Ok(())
                    });

                    match txn_res {
                        Ok(_) => {
                            // EOF
                            debug!("ok");
                        }
                        Err(sled::transaction::TransactionError::Abort(e)) => {
                            debug!("aborted txn: {}", e);
                            tx.send(e).await;
                        }
                        Err(sled::transaction::TransactionError::Storage(e)) => {
                            warn!("storage error in transaction: {}", e);
                            tx.send(Err(Status::internal("storage error in transaction")))
                                .await;
                        }
                    }
                });

                let receiver_stream = ReceiverStream::new(rx);
                Ok(Response::new(receiver_stream))
            }
        }
    }

    #[instrument(skip(self, _request))]
    async fn put(
        &self,
        _request: Request<Streaming<Directory>>,
    ) -> Result<Response<PutDirectoryResponse>> {
        warn!(NOT_IMPLEMENTED_MSG);
        Err(Status::unimplemented(NOT_IMPLEMENTED_MSG))
    }
}
