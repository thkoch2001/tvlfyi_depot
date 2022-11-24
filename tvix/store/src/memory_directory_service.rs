use futures::{Stream, StreamExt};
use std::collections::HashMap;
use std::pin::Pin;
use tokio::sync::RwLock;
use tonic::{Request, Response, Result, Status, Streaming};

use crate::proto::directory_service_server::DirectoryService;
use crate::proto::{Directory, GetDirectoryRequest, PutDirectoryResponse};

#[derive(Debug, Default)]
pub struct MemoryDirectoryService {
    directories: RwLock<HashMap<Vec<u8>, Directory>>,
}

#[tonic::async_trait]
/// DirectoryService stores things in a hashmap
impl DirectoryService for MemoryDirectoryService {
    type GetStream = Pin<Box<dyn Stream<Item = Result<Directory, Status>> + Send>>;

    async fn get(
        &self,
        _request: Request<GetDirectoryRequest>,
    ) -> Result<Response<Self::GetStream>, Status> {
        Err(Status::unimplemented(""))
    }

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
                            return Err(Status::failed_precondition(
                                "received directory contains unknown child directory",
                            ));
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
                    // TODO: log it
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
            for (k, v) in received_directories {
                let found = {
                    let directories_r = self.directories.blocking_read();
                    directories_r.contains_key(&k)
                };
                if !found {
                    let mut directories_w = self.directories.blocking_write();
                    directories_w.insert(k, v);
                }
            }

            return Ok(Response::new(PutDirectoryResponse {
                root_digest: last_d,
            }));
        } else {
            return Err(Status::invalid_argument("no directories received"));
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::memory_directory_service::MemoryDirectoryService;
    use crate::proto::directory_service_server::DirectoryService;
    use crate::proto::GetDirectoryRequest;

    #[tokio::test]
    async fn test_err() {
        let service = MemoryDirectoryService::default();

        // TODO: test get

        let result = service
            .get(tonic::Request::new(GetDirectoryRequest {
                by_what: None,
                recursive: false,
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
