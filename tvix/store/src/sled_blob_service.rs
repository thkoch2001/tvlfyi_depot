use crate::proto::blob_service_server::BlobService;
use crate::proto::BlobChunk;
use crate::proto::BlobMeta;
use crate::proto::PutBlobResponse;
use crate::proto::ReadBlobRequest;
use crate::proto::StatBlobRequest;
use prost::Message;
use std::path::PathBuf;
use tokio_stream::wrappers::ReceiverStream;
use tonic::{Request, Response, Result, Status, Streaming};
use tracing::{instrument, warn};

const NOT_IMPLEMENTED_MSG: &str = "not implemented";

pub struct SledBlobService {
    db: sled::Db,

    // tree containing the BlobMeta for a given Blob.
    tree_blobmeta: sled::Tree,

    // tree containing individual chunks.
    tree_chunks: sled::Tree,
}

/// SledBlobService uses [sled](https://github.com/spacejam/sled) to store blobs.
///
/// We maintain two trees in sled:
///  - `blobmeta` contains serialized [BlobMeta] messages, pointing to
///    individual chunks. The BlobMeta messages are keyed by the blake3 hash of the
///    blob contents.
///  - `chunks` contains individual chunk data, keyed by the blake3 hash of the
///    chunk contents.
///
/// For very small blobs, only a single chunk may be produced. We still create
/// a blobmeta entry, because clients might be interested in size or baos for
/// range requests.
///
/// TODO: What about not compressing chunks via sled, but on our own, and
/// signalling compression in [BlobChunk]? Most gRPC stacks only seem to
/// support gzip compression, and it doesn't make not a lot of sense to
/// decompress data on read, only to then recompress it while sending to the
/// client.
impl SledBlobService {
    pub fn new(p: PathBuf) -> Result<Self, anyhow::Error> {
        let config = sled::Config::default().use_compression(true).path(p);
        let db = config.open()?;

        Ok(Self {
            tree_blobmeta: db.clone().open_tree("blobmeta")?,
            tree_chunks: db.clone().open_tree("chunks")?,
            db,
        })
    }
}

#[tonic::async_trait]
impl BlobService for SledBlobService {
    type ReadStream = ReceiverStream<Result<BlobChunk>>;

    #[instrument(skip(self))]
    async fn stat(&self, request: Request<StatBlobRequest>) -> Result<Response<BlobMeta>> {
        let request = request.into_inner();

        // bail if include_bao is requested, that's not supported yet.
        if request.include_bao {
            return Err(Status::unimplemented("include_bao is not implemented yet."));
        }

        if request.digest.len() != 32 {
            return Err(Status::invalid_argument("invalid digest length in request"));
        }

        // if include_chunks is also false, the user only wants to know if the blob is present at all.
        if request.include_chunks == false {
            return match self.tree_blobmeta.contains_key(&request.digest) {
                Ok(contains) => {
                    if contains {
                        Ok(Response::new(BlobMeta {
                            ..Default::default()
                        }))
                    } else {
                        Err(Status::not_found("blob not found"))
                    }
                }
                Err(e) => Err(Status::internal(e.to_string())),
            };
        }

        // otherwise, look up chunk meta
        return match self.tree_blobmeta.get(&request.digest) {
            Ok(None) => Err(Status::not_found("blob not found")),
            Ok(Some(data)) => {
                // decode BlobMeta
                match BlobMeta::decode(&*data) {
                    Ok(blob_meta) => {
                        Ok(Response::new(BlobMeta {
                            // only serve chunks for now
                            chunks: blob_meta.chunks,
                            ..Default::default()
                        }))
                    }
                    Err(e) => Err(Status::internal(format!(
                        "unable to decode blobmeta: {}",
                        e
                    ))),
                }
            }
            Err(e) => Err(Status::internal(e.to_string())),
        };
    }

    #[instrument(skip(self))]
    async fn read(
        &self,
        _request: Request<ReadBlobRequest>,
    ) -> Result<Response<Self::ReadStream>, Status> {
        // TODO: check in blobmeta, and append smaller chunks.
        warn!(NOT_IMPLEMENTED_MSG);
        Err(Status::unimplemented(NOT_IMPLEMENTED_MSG))
    }

    #[instrument(skip(self, _request))]
    async fn put(
        &self,
        _request: Request<Streaming<BlobChunk>>,
    ) -> Result<Response<PutBlobResponse>> {
        warn!(NOT_IMPLEMENTED_MSG);
        Err(Status::unimplemented(NOT_IMPLEMENTED_MSG))
    }
}
