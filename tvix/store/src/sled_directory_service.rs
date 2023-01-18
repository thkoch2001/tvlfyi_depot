use base64::Engine;
use std::collections::VecDeque;
use std::path::PathBuf;

use prost::Message;
use sled::transaction::ConflictableTransactionError::Abort;
use tokio::sync::mpsc;
use tokio::sync::mpsc::Sender;
use tokio::task;
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

    /// Lookup a directory, optionally recurse, and send the result to the passed sender.
    /// This will open a sled txn to ensure a consistent view.
    fn get_directories(
        &self,
        tx: &Sender<Result<Directory>>,
        req: &GetDirectoryRequest,
    ) -> Result<(), Status> {
        let b64 = base64::engine::GeneralPurpose::new(
            &base64::alphabet::STANDARD,
            base64::engine::general_purpose::PAD,
        );

        // open a DB transaction.
        let txn_res = self.db.transaction(|txn| {
            // keep the list of directories to traverse
            let mut deq: VecDeque<Vec<u8>> = VecDeque::new();

            match &req.by_what {
                None => {
                    return Err(Abort(Err::<(), Status>(Status::unimplemented(
                        "by_what needs to be specified",
                    ))))
                }
                Some(ByWhat::Digest(digest)) => {
                    if digest.len() != 32 {
                        return Err(Abort(Err(Status::invalid_argument(
                            "invalid digest length",
                        ))));
                    }
                    deq.push_back(digest.clone());
                }
            }

            loop {
                // look up the directory at the top of the queue
                match deq.pop_front() {
                    // if there's nothing left, we're done
                    None => break Ok(()),
                    Some(ref digest) => {
                        let digest_b64: String = b64.encode(digest.clone());

                        match txn.get(digest) {
                            // The directory was not found, abort
                            Ok(None) => {
                                break Err(Status::not_found(format!(
                                    "directory {} not found",
                                    digest_b64
                                )))
                            }
                            // The directory was found, try to parse the data as Directory message
                            Ok(Some(data)) => match Directory::decode(&*data) {
                                Err(e) => {
                                    warn!("unable to parse directory {}: {}", digest_b64, e);
                                    break Err(Status::internal(format!(
                                        "unable to parse directory {}",
                                        digest_b64
                                    )));
                                }
                                Ok(directory) => {
                                    // TODO: validate the checksum matches to rule out storage errors?

                                    // if recursion was requested, all its children need to be added to the queue.
                                    if req.recursive {
                                        for child_directory_node in &directory.directories {
                                            deq.push_back(child_directory_node.digest.clone());
                                        }
                                    }

                                    // send the directory message to the client
                                    if let Err(e) = tx.blocking_send(Ok(directory)) {
                                        debug!("error sending: {}", e);
                                        // TODO: avoid trying to send this again?
                                        break Err(Status::internal("error sending"));
                                    }
                                }
                            },
                            // some storage error
                            Err(e) => {
                                warn!("storage error: {}", e);
                                break Err(Status::internal("storage error"));
                            }
                        };
                    }
                };
            }
            .map_err(|e| {
                // send all errors to the client
                tx.blocking_send(Err(e.clone()));

                // and pass the error along
                sled::transaction::ConflictableTransactionError::Abort(Err(e))
            })
        });

        match txn_res {
            Ok(()) => Ok(()),
            Err(sled::transaction::TransactionError::Abort(Ok(()))) => Ok(()),
            Err(sled::transaction::TransactionError::Abort(Err(e))) => Err(e),
            Err(sled::transaction::TransactionError::Storage(e)) => {
                warn!("storage error: {}", e);
                Err(Status::internal("storage error"))
            }
        }
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
        let req = request.into_inner();

        let (tx, rx) = mpsc::channel(10);
        // TODO: this needs to be reworked, we can't borrow self in here anymore.
        // move the entire "directory lookup logic" to a closure?
        task::spawn_blocking(move || match self.get_directories(&tx, &req) {
            Ok(_) => {}
            Err(e) => {
                tx.blocking_send(Err(e));
            }
        });

        let receiver_stream = ReceiverStream::new(rx);
        Ok(Response::new(receiver_stream))
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
