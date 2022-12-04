use futures::StreamExt;
use prost::Message;
use sled;
use sled::transaction::{abort, ConflictableTransactionError, TransactionError};
use std::collections::{HashMap, HashSet, VecDeque};
use std::convert::Infallible;
use std::path::PathBuf;
use tokio::sync::mpsc;
use tokio_stream::wrappers::ReceiverStream;
use tonic::{async_trait, Request, Response, Result, Status, Streaming};
use tracing::{debug, error, info_span, instrument, warn};

use crate::proto::directory_service_server::DirectoryService;
use crate::proto::{
    get_directory_request::ByWhat, Directory, GetDirectoryRequest, PutDirectoryResponse,
};

pub struct SledDirectoryService {
    db: sled::Db,
}

impl SledDirectoryService {
    pub fn new(p: PathBuf) -> Result<Self, anyhow::Error> {
        let db = sled::open(&p)?;

        Ok(Self { db })
    }
}
#[async_trait]
impl DirectoryService for SledDirectoryService {
    type GetStream = ReceiverStream<Result<Directory>>;

    #[instrument(skip(self))]
    async fn get(
        &self,
        request: Request<GetDirectoryRequest>,
    ) -> Result<Response<Self::GetStream>, Status> {
        let get_directory_request = request.into_inner();

        match get_directory_request.by_what {
            Some(ByWhat::Digest(root_digest)) => {
                let _ = info_span!("by_what", "{}", ByWhat::Digest(root_digest.clone()));

                let (tx, rx) = mpsc::channel::<Result<Directory>>(10);

                // Spawn a (blocking) thread that'll query the directories bucket.
                let db = self.db.clone();
                tokio::task::spawn_blocking(move || {
                    // We don't really make use of any of the "rollback properties" of a transaction,
                    // but use it to ensure a consistent view.
                    let res = db.transaction::<_, (), Status>(|db_txn| {
                        // Populate a queue of directory identifiers that need to be looked up
                        let mut directory_queue: VecDeque<Vec<u8>> =
                            VecDeque::from([root_digest.clone()]);
                        // Maintain a set of directory identifiers already sent. A directory closure might
                        // point to the same subgraph multiple times, and we only need to send it once.
                        let mut directory_digests_sent: HashSet<Vec<u8>> = HashSet::new();

                        loop {
                            // pick the next directory digest from the queue.
                            match directory_queue.pop_front() {
                                // if there's nothing left to return, we're done.
                                None => {
                                    break Ok(());
                                }
                                Some(dgst) => {
                                    let _ =
                                        info_span!("digest", "{}", base64::encode(dgst.clone()));
                                    debug!("Found directory.");
                                    // check if that digest has already been sent.
                                    // If it was, continue with the next.
                                    if directory_digests_sent.contains(&dgst) {
                                        continue;
                                    }

                                    match db_txn.get(&dgst) {
                                        // Directory not found
                                        Ok(None) => {
                                            let msg = "Unable to find referred directory.";
                                            error!(msg);
                                            return abort(Status::not_found(msg));
                                        }
                                        // Directory found
                                        Ok(Some(d_bytes)) => {
                                            // parse directory
                                            // TODO: unwrap

                                            let d: Directory = match Directory::decode(&*d_bytes) {
                                                Ok(d) => d,
                                                Err(e) => {
                                                    error!("failed to decode directory proto: {e}");
                                                    return Err(
                                                        ConflictableTransactionError::Storage(
                                                            sled::Error::Io(e.into()),
                                                        ),
                                                    );
                                                }
                                            };

                                            // If recursive lookup was requested, add all child directory digest to the queue.
                                            if get_directory_request.recursive {
                                                directory_queue.extend(
                                                    d.directories
                                                        .iter()
                                                        .map(|x| x.digest.clone())
                                                        .collect::<VecDeque<Vec<u8>>>(),
                                                );
                                            }

                                            // send out the current directory
                                            if let Err(err) = tx.blocking_send(Ok(d.clone())) {
                                                let msg = "Error sending directory.";
                                                error!("{} {:?}", msg, err);
                                                return abort(Status::internal(msg));
                                            }
                                            directory_digests_sent.insert(dgst);
                                        }
                                        // Error during lookup
                                        Err(err) => {
                                            let msg = "Error during directory lookup.";
                                            error!("{} {:?}", msg, err);
                                            return abort(Status::internal(msg));
                                        }
                                    }
                                }
                            };
                        }
                    });

                    match res {
                        Ok(()) => {}
                        Err(e) => {
                            match e {
                                TransactionError::Abort(a) => tx.blocking_send(Err(a)),
                                TransactionError::Storage(s) => {
                                    error!("storage error: {:?}", s);
                                    tx.blocking_send(Err(Status::data_loss("storage error")))
                                }
                            }
                            .unwrap_or_else(|e| {
                                error!("failed to send error to client: {e}");
                            });
                        }
                    }
                });

                let receiver_stream = ReceiverStream::new(rx);
                Ok(Response::new(receiver_stream))
            }
            None => {
                let msg = "The by_what field needs to be present and contain Digest.";
                warn!("Invalid argument. {}", msg);
                Err(Status::invalid_argument(msg))
            }
        }
    }

    #[instrument(skip(self, request))]
    async fn put(
        &self,
        request: Request<Streaming<Directory>>,
    ) -> Result<Response<PutDirectoryResponse>, Status> {
        let mut directory_stream = request.into_inner();
        let mut received_directories = HashMap::<Vec<u8>, Directory>::new();

        // keep a reference to the last directory received
        let mut last_directory_digest: Option<Vec<u8>> = None;

        // Receive all directory messages that are sent one by one
        loop {
            match directory_stream.next().await {
                Some(Ok(directory)) => {
                    // for each referenced directory, we need to check if it has been sent in the stream
                    for child_directory in &directory.directories {
                        if !received_directories.contains_key(&child_directory.digest) {
                            let msg = "Received directory contains unknown child directory.";
                            warn!(%child_directory, %directory, "Failed precondition. {}", msg);
                            return Err(Status::failed_precondition(msg));
                        }
                    }
                    // calculate the digest of the directory before it gets partially moved during iter
                    let dgst = directory.clone().digest();
                    debug!(digest = base64::encode(dgst.clone()), "Received directory.");

                    // all good, add directory to received_directories and last_directory
                    received_directories.insert(dgst.clone(), directory);
                    last_directory_digest = Some(dgst);
                }
                Some(Err(err)) => {
                    // error from the client. We simply bounce it back :-P
                    warn!(?err, "Received error in directory stream.");
                    return Err(err);
                }
                // end of the stream
                None => {
                    break;
                }
            };
        }

        // we need to have received at least one directory, to be able to return the root digest of the tree that has been sent.
        match last_directory_digest {
            None => {
                let msg = "No directories received.";
                warn!("Invalid argument. {}", msg);
                return Err(Status::invalid_argument(msg));
            }
            Some(last_d) => {
                // insert received_directories into the store
                // we can skip the ones that already exist because things are content-addressed
                let resp = self.db.transaction::<_, (), Infallible>(|tx_db| {
                    for (k, v) in &received_directories {
                        let _ = info_span!("digest", "{}", base64::encode(k.clone()));

                        // check if the directory already exists in the db
                        if tx_db.get(&k)?.is_some() {
                            debug!("Directory already exists. Ignoring");
                        } else {
                            debug!("Inserting directory.");
                            tx_db.insert(&k[..], v.encode_to_vec())?;
                        }
                    }
                    Ok(())
                });

                match resp {
                    Ok(_) => Ok(Response::new(PutDirectoryResponse {
                        root_digest: last_d,
                    })),
                    Err(TransactionError::Abort(e)) => match e {},
                    Err(TransactionError::Storage(e)) => {
                        error!("storage error: {e}");
                        Err(Status::internal("storage error"))
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::proto::directory_service_server::DirectoryService;
    use crate::proto::get_directory_request::ByWhat::Digest;
    use crate::proto::{Directory, GetDirectoryRequest};
    use crate::sled_directory_service::SledDirectoryService;
    use tempfile::TempDir;
    use tokio_stream::StreamExt;

    #[tokio::test]
    // Put an empty directory and then get it
    async fn test_put_get() -> anyhow::Result<()> {
        let tmpdir = TempDir::new()?;
        let service = SledDirectoryService::new(tmpdir.path().into())?;
        let directory = Directory {
            directories: vec![],
            files: vec![],
            symlinks: vec![],
        };

        let streaming_request = tonic_mock::streaming_request(vec![directory.clone()]);
        let put_response = service.put(streaming_request).await.unwrap().into_inner();
        assert_eq!(put_response.root_digest.len(), 32);

        let mut get_stream = service
            .get(tonic::Request::new(dbg!(GetDirectoryRequest {
                by_what: Some(Digest(put_response.root_digest)),
                recursive: false,
            })))
            .await
            .unwrap()
            .into_inner();
        let mut get_result = Vec::new();
        while let Some(v) = get_stream.next().await {
            get_result.push(v)
        }
        assert_eq!(get_result.len(), 1);
        assert_eq!(get_result.get(0).unwrap().as_ref().unwrap(), &directory);
        Ok(())
    }

    #[tokio::test]
    // Get with invalid request
    async fn test_empty_by_what() -> anyhow::Result<()> {
        let tmpdir = TempDir::new()?;
        let service = SledDirectoryService::new(tmpdir.path().into())?;
        let result = service
            .get(tonic::Request::new(GetDirectoryRequest {
                by_what: None,
                recursive: false,
            }))
            .await;
        assert!(result.is_err());
        if let Err(status) = result {
            assert_eq!(status.code(), tonic::Code::InvalidArgument)
        } else {
            assert!(false)
        }
        Ok(())
    }
}
