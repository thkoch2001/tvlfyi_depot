use crate::proto::blob_meta::ChunkMeta;
use crate::proto::blob_service_server::BlobService;
use crate::proto::BlobChunk;
use crate::proto::BlobMeta;
use crate::proto::PutBlobResponse;
use crate::proto::ReadBlobRequest;
use crate::proto::StatBlobRequest;
use data_encoding::BASE64;
use prost::Message;
use std::path::PathBuf;
use tokio::sync::mpsc::channel;
use tokio::task;
use tokio_stream::wrappers::ReceiverStream;
use tonic::{Request, Response, Result, Status, Streaming};
use tracing::{instrument, warn};

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
///    chunk contents (content-addressed)
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
        request: Request<ReadBlobRequest>,
    ) -> Result<Response<Self::ReadStream>, Status> {
        let (tx, rx) = channel(5);

        let req_inner = request.into_inner();

        let tree_blobmeta = self.tree_blobmeta.clone();
        let tree_chunks = self.tree_chunks.clone();

        /// NOTE: this currently sends one chunk per message.
        // Depending on what chunking size we end up with, we might want to
        // split chunks during streaming.
        task::spawn_blocking(move || {
            let blob_digest_b64 = BASE64.encode(&req_inner.digest);
            // look up in blobmeta
            match tree_blobmeta.get(&req_inner.digest) {
                Err(e) => {
                    tx.blocking_send(Err(Status::internal(format!(
                        "failed to get blobmeta for {}: {}",
                        blob_digest_b64, e,
                    ))));
                }
                Ok(None) => {
                    // We didn't find this blob in blobmeta, but there might be an individual chunk
                    match tree_chunks.get(&req_inner.digest) {
                        Ok(None) => {
                            // Neither in blobmeta, nor an individual chunk
                            tx.blocking_send(Err(Status::not_found(format!(
                                "blob {} not found",
                                blob_digest_b64,
                            ))));
                        }
                        Ok(Some(data)) => {
                            tx.blocking_send(Ok(BlobChunk {
                                data: data.to_vec(),
                            }));
                        }
                        Err(e) => {
                            tx.blocking_send(Err(Status::internal(format!(
                                "failed to get chunk for blob {}: {}",
                                blob_digest_b64, e
                            ))));
                        }
                    }
                }
                Ok(Some(data)) => match BlobMeta::decode(&*data) {
                    Err(e) => {
                        warn!("unable to parse blobmeta for {}: {}", blob_digest_b64, e);
                        tx.blocking_send(Err(Status::internal(format!(
                            "unable to parse blobmeta for {}",
                            blob_digest_b64
                        ))));
                    }
                    Ok(blobmeta) => {
                        for (i, chunk) in blobmeta.chunks.iter().enumerate() {
                            // lookup chunk from tree_chunks
                            match tree_chunks.get(&chunk.digest) {
                                Ok(None) => {
                                    tx.blocking_send(Err(Status::not_found(format!(
                                        "chunk {} for blob {} not found",
                                        i, blob_digest_b64,
                                    ))));
                                }
                                Ok(Some(data)) => {
                                    tx.blocking_send(Ok(BlobChunk {
                                        data: data.to_vec(),
                                    }));
                                }
                                Err(e) => {
                                    tx.blocking_send(Err(Status::internal(format!(
                                        "failed to get chunk {} for blob {}: {}",
                                        i, blob_digest_b64, e
                                    ))));
                                }
                            }
                        }
                    }
                },
            }
        });

        // look up the blob in chunkmeta
        let receiver_stream = ReceiverStream::new(rx);
        Ok(Response::new(receiver_stream))
    }

    #[instrument(skip(self, request))]
    async fn put(
        &self,
        request: Request<Streaming<BlobChunk>>,
    ) -> Result<Response<PutBlobResponse>> {
        let mut req_inner = request.into_inner();

        // TODO: for now, we collect all Chunks into a large Vec<u8>, and then
        // pass it to a (content-defined) Chunker.
        // This is because the fastcdc crate currently operates on byte slices,
        // not on something implementing [std::io::Read].
        // (see https://github.com/nlfiedler/fastcdc-rs/issues/17)

        let mut blob_contents: Vec<u8> = Vec::new();

        while let Some(mut blob_chunk) = req_inner.message().await? {
            blob_contents.append(&mut blob_chunk.data);
        }

        // initialize a new chunker
        // TODO: play with chunking sizes
        let chunker = fastcdc::FastCDC::new(
            &blob_contents,
            64 * 1024 / 4, // min
            64 * 1024,     // avg
            64 * 1024 * 4, // max
        );

        // initialize blake3 hashers. chunk_hasher is used and reset for each
        // chunk, blob_hasher calculates the hash of the whole blob.
        let mut chunk_hasher = blake3::Hasher::new();
        let mut blob_hasher = blake3::Hasher::new();

        // start a BlobMeta, which we'll fill while looping over the chunks
        let mut blobmeta = BlobMeta::default();

        // loop over all the chunks
        for chunk in chunker {
            // extract the data itself
            let chunk_data: Vec<u8> =
                blob_contents[chunk.offset..chunk.offset + chunk.length].to_vec();

            // calculate the digest of that chunk
            chunk_hasher.update(&chunk_data);
            let chunk_digest = chunk_hasher.finalize();
            chunk_hasher.reset();

            // also update blob_hasher
            blob_hasher.update(&chunk_data);

            // insert chunk into db if it doesn't exist already
            match self.tree_chunks.contains_key(chunk_digest.as_bytes()) {
                Ok(contains) => {
                    if !contains {
                        if let Err(e) = self
                            .tree_chunks
                            .insert(chunk_digest.as_bytes().clone(), chunk_data)
                        {
                            return Err(Status::internal(format!("failed to insert blob: {}", e)));
                        }
                    }
                }
                Err(e) => {
                    return Err(Status::internal(format!(
                        "unable to check if chunk exists: {}",
                        e
                    )))
                }
            }

            // add chunk to blobmeta
            blobmeta.chunks.push(ChunkMeta {
                digest: chunk_digest.as_bytes().to_vec(),
                size: chunk.length as u32,
            })
        }

        let blob_digest = blob_hasher.finalize().as_bytes().to_vec();

        // store blobmeta
        // TODO: don't store if we already have it (potentially with different chunking)
        match self
            .tree_blobmeta
            .insert(blob_digest.clone(), blobmeta.encode_to_vec())
        {
            Ok(_) => Ok(Response::new(PutBlobResponse {
                digest: blob_digest,
            })),
            Err(e) => Err(Status::internal(format!(
                "unable to insert blobmeta: {}",
                e
            ))),
        }
    }
}
