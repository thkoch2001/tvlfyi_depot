use std::{
    io::{self},
    sync::Arc,
};

use data_encoding::HEXLOWER;
use object_store::{path::Path, ObjectStore};
use prost::Message;
use tonic::async_trait;
use tracing::{debug, instrument, Level};
use url::Url;

use crate::{proto::StatBlobResponse, B3Digest};

use super::{BlobService, ChunkMeta};

#[derive(Clone)]
pub struct ObjectStoreBlobService {
    object_store: Arc<dyn ObjectStore>,
    base_path: Path,

    /// Average chunk size for FastCDC, in bytes.
    /// min value is half, max value double of that number.
    avg_chunk_size: u32,
}

/// Uses any object storage supported by the [object_store] crate to provide a
/// tvix-castore [BlobService].
///
/// # Data format
/// Data is organized in "blobs" and "chunks".
/// Blobs don't hold the actual data, but instead contain a list of more
/// granular chunks that assemble to the contents requested.
/// This allows clients to seek, and not download chunks they already have
/// locally, as it's referred to from other files.
/// Check `rpc_blobstore` and more general BlobStore docs on that.
///
/// ## Blobs
/// Stored at `${base_path}/blobs/b3/$digest_key`. They contains the serialized
/// StatBlobResponse for the blob with the digest.
///
/// ## Chunks
/// Chunks are stored at `${base_path}/chunks/b3/$digest_key`. They contain
/// the literal contents of the chunk, but are zstd-compressed.
///
/// ## Digest key sharding
/// The blake3 digest encoded in lower hex, and sharded after the second
/// character.
/// The blob for "Hello World" is stored at
/// `${base_path}/blobs/b3/41/41f8394111eb713a22165c46c90ab8f0fd9399c92028fd6d288944b23ff5bf76`.
///
/// This reduces the number of files in the same directory, which would be a
/// problem at least when using [object_store::local::LocalFileSystem].
///
/// # Future changes
/// There's no guarantees about this being a final format yet.
/// Once object_store gets support for additional metadata / content-types,
/// we can eliminate some requests (small blobs only consisting of a single
/// chunk can be stored as-is, without the blob index file).
/// It also allows signalling any compression of chunks in the content-type.
/// Migration *should* be possible by simply adding the right content-types to
/// all keys stored so far, but no promises ;-)
impl ObjectStoreBlobService {
    /// Constructs a new [ObjectStoreBlobService] from a [Url] supported by
    /// [object_store].
    /// Any path suffix becomes the base path of the object store.
    /// additional options, the same as in [object_store::parse_url_opts] can
    /// be passed.
    pub fn parse_url_opts<I, K, V>(url: &Url, options: I) -> Result<Self, object_store::Error>
    where
        I: IntoIterator<Item = (K, V)>,
        K: AsRef<str>,
        V: Into<String>,
    {
        let (object_store, path) = object_store::parse_url_opts(url, options)?;

        Ok(Self {
            object_store: Arc::new(object_store),
            base_path: path,
            avg_chunk_size: 256 * 1024,
        })
    }

    /// Like [Self::parse_url_opts], except without the options.
    pub fn parse_url(url: &Url) -> Result<Self, object_store::Error> {
        Self::parse_url_opts(url, Vec::<(String, String)>::new())
    }
}

#[instrument(level=Level::TRACE, skip_all,fields(base_path=%base_path,blob.digest=%digest),ret(Display))]
fn derive_blob_path(base_path: &Path, digest: &B3Digest) -> Path {
    base_path
        .child("blobs")
        .child("b3")
        .child(HEXLOWER.encode(&digest.as_slice()[..2]))
        .child(HEXLOWER.encode(digest.as_slice()))
}

#[instrument(level=Level::TRACE, skip_all,fields(base_path=%base_path,chunk.digest=%digest),ret(Display))]
fn derive_chunk_path(base_path: &Path, digest: &B3Digest) -> Path {
    base_path
        .child("chunks")
        .child("b3")
        .child(HEXLOWER.encode(&digest.as_slice()[..2]))
        .child(HEXLOWER.encode(digest.as_slice()))
}

#[async_trait]
impl BlobService for ObjectStoreBlobService {
    #[instrument(skip_all, ret, err, fields(blob.digest=%digest))]
    async fn has(&self, digest: &B3Digest) -> io::Result<bool> {
        // TODO: clarify if this should work for chunks or not, and explicitly
        // document in the proto docs.
        let p = derive_blob_path(&self.base_path, digest);

        match self.object_store.head(&p).await {
            Ok(_) => Ok(true),
            Err(object_store::Error::NotFound { .. }) => {
                let p = derive_chunk_path(&self.base_path, digest);
                match self.object_store.head(&p).await {
                    Ok(_) => Ok(true),
                    Err(object_store::Error::NotFound { .. }) => Ok(false),
                    Err(e) => Err(e)?,
                }
            }
            Err(e) => Err(e)?,
        }
    }

    #[instrument(skip_all, ret, err, fields(blob.digest=%digest))]
    async fn put(&self, digest: B3Digest, chunks: &[ChunkMeta]) -> io::Result<()> {
        let stat_blob_response = StatBlobResponse {
            chunks: chunks.iter().cloned().map(Into::into).collect(),
            bao: "".into(), // still todo
        };

        let blob_path = derive_blob_path(&self.base_path, &digest);

        self.object_store
            .put(&blob_path, stat_blob_response.encode_to_vec().into())
            .await?;

        Ok(())
    }

    #[instrument(skip_all, err, fields(blob.digest=%digest))]
    async fn chunks(&self, digest: &B3Digest) -> io::Result<Option<Vec<ChunkMeta>>> {
        match self
            .object_store
            .get(&derive_blob_path(&self.base_path, digest))
            .await
        {
            Ok(get_result) => {
                // fetch the data at the blob path
                let blob_data = get_result.bytes().await?;
                // parse into StatBlobResponse
                let stat_blob_response: StatBlobResponse = StatBlobResponse::decode(blob_data)?;

                debug!(
                    chunk.count = stat_blob_response.chunks.len(),
                    blob.size = stat_blob_response
                        .chunks
                        .iter()
                        .map(|x| x.size)
                        .sum::<u64>(),
                    "found more granular chunks"
                );

                let chunks = stat_blob_response
                    .chunks
                    .into_iter()
                    .map(|chunk| chunk.try_into())
                    .collect::<Result<_, _>>()
                    .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))?;

                Ok(Some(chunks))
            }
            Err(object_store::Error::NotFound { .. }) => {
                // If there's only a chunk, we must return the empty vec here, rather than None.
                match self
                    .object_store
                    .head(&derive_chunk_path(&self.base_path, digest))
                    .await
                {
                    Ok(_) => {
                        // present, but no more chunks available
                        debug!("found a single chunk");
                        Ok(Some(vec![]))
                    }
                    Err(object_store::Error::NotFound { .. }) => {
                        // Neither blob nor single chunk found
                        debug!("not found");
                        Ok(None)
                    }
                    // error checking for chunk
                    Err(e) => Err(e.into()),
                }
            }
            // error checking for blob
            Err(err) => Err(err.into()),
        }
    }
}
