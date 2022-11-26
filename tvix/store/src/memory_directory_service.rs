use futures::StreamExt;
use std::collections::{HashMap, HashSet, VecDeque};
use std::sync::Arc;
use tokio::sync::{mpsc, RwLock};
use tokio_stream::wrappers::ReceiverStream;
use tonic::{Request, Response, Result, Status, Streaming};
use tracing::{debug, error, info_span, instrument, warn};

use crate::proto::directory_service_server::DirectoryService;
use crate::proto::{
    get_directory_request::ByWhat, Directory, GetDirectoryRequest, PutDirectoryResponse,
};

#[derive(Debug, Default)]
pub struct MemoryDirectoryService {
    directories: Arc<RwLock<HashMap<Vec<u8>, Directory>>>,
}

#[tonic::async_trait]
/// DirectoryService stores things in a hashmap
impl DirectoryService for MemoryDirectoryService {
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

                let (tx, rx) = mpsc::channel(10);

                let mut directory_queue: VecDeque<Vec<u8>> = VecDeque::from([root_digest.clone()]);

                // clone self.directories, so we can use it inside the closure
                let directories = self.directories.clone();

                tokio::spawn(async move {
                    let mut directory_digests_sent: HashSet<Vec<u8>> = HashSet::new();

                    loop {
                        // check for directories to return
                        match directory_queue.pop_front() {
                            Some(dgst) => {
                                let _ = info_span!("digest", "{}", base64::encode(dgst.clone()));
                                debug!("Found digest.");
                                // check if that digest has already been sent.
                                // If it was, continue with the next.
                                // This can be the case if multiple directories in a closure point to the same contents.
                                if directory_digests_sent.contains(&dgst) {
                                    continue;
                                }
                                // else, look it up.
                                // FUTUREWORK: We currently do acquire the read lock here, when looking up each individual directory.
                                // This means we don't have a guarantee the whole directory closure is still there, if a GC has
                                // removed some of the directories while we're processing the current request.
                                let directories_r = directories.read().await;
                                if let Some(d) = directories_r.get(&dgst) {
                                    if get_directory_request.recursive {
                                        // add all child directory digests to the list of directories to send if we're in recursive mode
                                        directory_queue.extend(
                                            d.directories
                                                .iter()
                                                .map(|x| x.digest.clone())
                                                .collect::<VecDeque<Vec<u8>>>(),
                                        );
                                    }

                                    // send out the current directory
                                    if let Err(err) = tx.send(Ok(d.clone())).await {
                                        let msg = "Error sending directory.";
                                        error!("{} {:?}", msg, err);
                                        break;
                                    };
                                    directory_digests_sent.insert(dgst);
                                } else {
                                    let msg = "Unable to find referred directory.";
                                    error!(msg);
                                    tx.send(Err(Status::internal(msg))).await.ok();
                                    break;
                                }
                            }
                            None => {
                                // if there's nothing left to return, break the loop
                                break;
                            }
                        };
                    }
                    ()
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
                    debug!("Got directory: {:?}", directory);
                    // for each referenced directory, we need to check if it has been sent in the stream
                    for child_directory in &directory.directories {
                        if !received_directories.contains_key(&child_directory.digest) {
                            let msg = "Received directory contains unknown child directory.";
                            warn!("Failed precondition. {}", msg);
                            return Err(Status::failed_precondition(msg));
                        }
                    }
                    // calculate the digest of the directory before it gets partially moved during iter
                    let dgst = directory.clone().digest();

                    // all good, add directory to received_directories and last_directory
                    received_directories.insert(dgst.clone(), directory);
                    last_directory_digest = Some(dgst);
                }
                Some(Err(e)) => {
                    // error from the client. We simply bounce it back :-P
                    warn!("Received error in directory stream: {:?}", e);
                    return Err(e);
                }
                // end of the stream
                None => {
                    break;
                }
            };
        }

        // we need to have received at least one directory, to be able to return the root digest of the tree that has been sent.
        if let Some(last_d) = last_directory_digest {
            // merge received_directories with the global hashmap
            debug!("Received directories: {:?}", received_directories);
            for (k, v) in received_directories {
                let found = {
                    let directories_r = self.directories.read().await;
                    directories_r.contains_key(&k)
                };
                if !found {
                    debug!("Inserting directory: {}", hex::encode(k.clone()));
                    let mut directories_w = self.directories.write().await;
                    directories_w.insert(k, v);
                }
            }

            return Ok(Response::new(PutDirectoryResponse {
                root_digest: last_d,
            }));
        } else {
            let err = "No directories received.";
            warn!("Invalid argument. {}", err);
            return Err(Status::invalid_argument(err));
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::memory_directory_service::MemoryDirectoryService;
    use crate::proto::directory_service_server::DirectoryService;
    use crate::proto::get_directory_request::ByWhat::Digest;
    use crate::proto::{Directory, GetDirectoryRequest};
    use tokio_stream::StreamExt;

    #[tokio::test]
    // Put an empty directory and then get it
    async fn test_put_get() {
        let service = MemoryDirectoryService::default();
        let directory = Directory {
            directories: vec![],
            files: vec![],
            symlinks: vec![],
        };

        let streaming_request = tonic_mock::streaming_request(vec![directory.clone()]);
        let put_response = service.put(streaming_request).await.unwrap().into_inner();
        assert_eq!(put_response.root_digest.len(), 32);

        let mut get_stream = service
            .get(tonic::Request::new(GetDirectoryRequest {
                by_what: Some(Digest(put_response.root_digest)),
                recursive: false,
            }))
            .await
            .unwrap()
            .into_inner();
        let mut get_result = Vec::new();
        while let Some(v) = get_stream.next().await {
            get_result.push(v)
        }
        assert_eq!(get_result.len(), 1);
        assert_eq!(get_result.get(0).unwrap().as_ref().unwrap(), &directory);
    }

    #[tokio::test]
    // Get with invalid request
    async fn test_empty_by_what() {
        let service = MemoryDirectoryService::default();
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
    }
}
