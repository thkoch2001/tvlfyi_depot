use crate::nar::StoreClient;
use crate::proto;
use crate::proto::get_path_info_request::ByWhat;
use crate::proto::path_info_service_server::PathInfoService;
use crate::proto::CalculateNarResponse;
use crate::proto::GetPathInfoRequest;
use crate::proto::PathInfo;
use count_write::CountWrite;
use nix_compat::store_path::DIGEST_SIZE;
use prost::Message;
use sha2::Digest;
use sha2::Sha256;
use std::path::PathBuf;
use tonic::{Request, Response, Result, Status};
use tracing::{instrument, warn};

/// SledPathInfoService stores PathInfo in a [sled](https://github.com/spacejam/sled).
///
/// The PathInfo messages are stored as encoded protos, and keyed by their output hash,
/// as that's currently the only request type available.
pub struct SledPathInfoService {
    db: sled::Db,

    store_client: Box<dyn StoreClient>,
}

impl SledPathInfoService {
    pub fn new(p: PathBuf, store_client: impl StoreClient) -> Result<Self, anyhow::Error> {
        let config = sled::Config::default().use_compression(true).path(p);
        let db = config.open()?;

        Ok(Self {
            db,
            store_client: Box::new(store_client),
        })
    }
}

#[tonic::async_trait]
impl PathInfoService for SledPathInfoService {
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
            Err(e) => Err(Status::invalid_argument(e.to_string())),
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
            None => Err(Status::invalid_argument("no root node sent")),
            Some(rq_root_node) => {
                let h = Sha256::new();
                let mut cw = CountWrite::from(h);

                let write_nar_result =
                    crate::nar::write_nar(&mut cw, rq_root_node, self.store_client.as_mut());

                if let Err(e) = write_nar_result {
                    return Err(Status::internal(format!("failed to render NAR: {}", e)));
                }

                Ok(Response::new(CalculateNarResponse {
                    nar_size: cw.count() as u32,
                    nar_sha256: cw.into_inner().finalize().to_vec(),
                }))
            }
        }
    }
}
