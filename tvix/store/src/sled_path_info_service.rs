use prost::Message;
use sha2::Digest;
use sha2::Sha256;
use std::path::PathBuf;

use crate::nixpath::DIGEST_SIZE;
use crate::proto;
use crate::proto::get_path_info_request::ByWhat;
use crate::proto::path_info_service_server::PathInfoService;
use crate::proto::CalculateNarResponse;
use crate::proto::GetPathInfoRequest;
use crate::proto::PathInfo;
use count_write::CountWrite;
use tonic::{Request, Response, Result, Status};
use tracing::{instrument, warn};
use tvix_nar;

const NOT_IMPLEMENTED_MSG: &str = "not implemented";

/// SledPathInfoService stores PathInfo in a [sled](https://github.com/spacejam/sled).
///
/// The PathInfo messages are stored as encoded protos, and keyed by their output hash,
/// as that's currently the only request type available.
pub struct SledPathInfoService<C: std::marker::Send> {
    db: sled::Db,

    blob_service_client: crate::proto::blob_service_client::BlobServiceClient<C>,
    directory_service_client: crate::proto::directory_service_client::DirectoryServiceClient<C>,
}

impl<C: Send + Sync + 'static> SledPathInfoService<C> {
    pub fn new(
        p: PathBuf,
        blob_service_client: crate::proto::blob_service_client::BlobServiceClient<C>,
        directory_service_client: crate::proto::directory_service_client::DirectoryServiceClient<C>,
    ) -> Result<Self, anyhow::Error> {
        let config = sled::Config::default().use_compression(true).path(p);
        let db = config.open()?;

        Ok(Self {
            db,
            blob_service_client,
            directory_service_client,
        })
    }

    fn walk_node(
        &self,
        nar_node: tvix_nar::Node,
        proto_node: proto::node::Node,
    ) -> Result<(), Status> {
        match proto_node {
            proto::node::Node::Directory(_directory_node) => {
                // ask the directory_service_client for all directory messages
                // (we can use recursive here and put all in a HashSet)

                // let nar_node_directory = nar_node.directory()?;

                // enumerate over the three different lists directory_node, pick the smallest one
                // TODO: do we want an iterator for that on Directory, returning proto::node::Node ?

                // for each of these, invoke
                // nar_node_directory.entry("the_name") and pass the result along to walk_node
                warn!(NOT_IMPLEMENTED_MSG);
                return Err(Status::unimplemented(NOT_IMPLEMENTED_MSG));
            }
            proto::node::Node::File(file_node) => {
                // TODO: implement reader for blob client?
                nar_node.file(file_node.executable, file_node.size.into(), todo!("reader"))?;
                Ok(())
            }
            proto::node::Node::Symlink(symlink_node) => {
                nar_node.symlink(&symlink_node.target)?;

                Ok(())
            }
        }
    }
}

#[tonic::async_trait]
impl<C: Send + Sync + 'static> PathInfoService for SledPathInfoService<C> {
    #[instrument(skip(self))]
    async fn get(&self, request: Request<GetPathInfoRequest>) -> Result<Response<PathInfo>> {
        match request.into_inner().by_what {
            None => Err(Status::unimplemented("by_what needs to be specified")),
            Some(ByWhat::ByOutputHash(digest)) => {
                if digest.len() != DIGEST_SIZE {
                    return Err(Status::invalid_argument("invalid digest length"));
                }

                match self.db.get(digest) {
                    Ok(None) => Err(Status::not_found("PathInfo not found")),
                    Ok(Some(data)) => match PathInfo::decode(&*data) {
                        Ok(path_info) => Ok(Response::new(path_info)),
                        Err(e) => {
                            warn!("failed to decode stored PathInfo: {}", e);
                            Err(Status::internal("failed to decode stored PathInfo"))
                        }
                    },
                    Err(e) => {
                        warn!("failed to retrieve PathInfo: {}", e);
                        Err(Status::internal("error during PathInfo lookup"))
                    }
                }
            }
        }
    }

    #[instrument(skip(self))]
    async fn put(&self, request: Request<PathInfo>) -> Result<Response<PathInfo>> {
        let path_info = request.into_inner();

        // Call validate on the received PathInfo message.
        match path_info.validate() {
            Err(e) => match e {
                crate::proto::ValidatePathInfoError::NoNodePresent() => {
                    Err(Status::invalid_argument("no node present"))
                }
                crate::proto::ValidatePathInfoError::InvalidNodeName(_, _) => {
                    Err(Status::invalid_argument("invalid node name"))
                }
                crate::proto::ValidatePathInfoError::InvalidDigestLen(_) => {
                    Err(Status::invalid_argument("invalid digest length"))
                }
                crate::proto::ValidatePathInfoError::InconsistentNumberOfReferences(_, _) => Err(
                    Status::invalid_argument("inconsistent number of references"),
                ),
            },
            // In case the PathInfo is valid, and we were able to extract a NixPath, store it in the database.
            // This overwrites existing PathInfo objects.
            Ok(nix_path) => match self.db.insert(nix_path.digest, path_info.encode_to_vec()) {
                Ok(_) => Ok(Response::new(path_info)),
                Err(e) => {
                    warn!("failed to insert PathInfo: {}", e);
                    Err(Status::internal("failed to insert PathInfo"))
                }
            },
        }
    }

    #[instrument(skip(self))]
    async fn calculate_nar(
        &self,
        request: Request<proto::Node>,
    ) -> Result<Response<CalculateNarResponse>> {
        match request.into_inner().node {
            None => Err(Status::invalid_argument("no root node present")),
            Some(rq_root_node) => {
                let h = Sha256::new();
                let mut cw = CountWrite::from(h);
                {
                    let nar_root_node = tvix_nar::open(&mut cw)?;

                    self.walk_node(nar_root_node, rq_root_node)?;
                }
                Ok(Response::new(CalculateNarResponse {
                    nar_size: cw.count() as u32,
                    nar_sha256: cw.into_inner().finalize().to_vec(),
                }))
            }
        }
    }
}
