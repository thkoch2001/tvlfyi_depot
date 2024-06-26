use data_encoding::HEXLOWER;
use object_store::path::Path;
use std::{
    io::{self, Cursor},
    sync::Arc,
};
use tonic::async_trait;
use tracing::{instrument, Level};
use url::Url;

use crate::B3Digest;

use super::{b3_hash, ChunkStore};

/// Uses any object storage supported by the [object_store] crate to store
/// chunks.
///
/// # Data format
/// Chunks are stored at `${base_path}/chunks/b3/$digest_key`.
/// They contain the literal contents of the chunk, but are zstd-compressed.
///
/// # Digest key sharding
/// The blake3 digest encoded in lower hex, and sharded after the second
/// character.
/// The chunk for "Hello World" is stored at
/// `${base_path}/chunks/b3/41/41f8394111eb713a22165c46c90ab8f0fd9399c92028fd6d288944b23ff5bf76`.
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
pub struct ObjectStore {
    object_store: Arc<dyn object_store::ObjectStore>,
    base_path: Path,
}

impl ObjectStore {
    /// Constructs a new [ObjectStore] from a [Url] supported by
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
        })
    }

    /// Like [Self::parse_url_opts], except without the options.
    pub fn parse_url(url: &Url) -> Result<Self, object_store::Error> {
        Self::parse_url_opts(url, Vec::<(String, String)>::new())
    }
}

#[async_trait]
impl ChunkStore for ObjectStore {
    #[instrument(skip_all, err, fields(chunk.digest=%digest))]
    async fn has(&self, digest: &B3Digest) -> io::Result<bool> {
        match self
            .object_store
            .head(&derive_chunk_path(&self.base_path, digest))
            .await
        {
            Ok(_) => Ok(true),
            Err(object_store::Error::NotFound { .. }) => Ok(false),
            Err(e) => Err(e.into()),
        }
    }

    #[instrument(skip_all, err, fields(chunk.digest=%digest))]
    async fn get(&self, digest: &B3Digest) -> io::Result<Option<bytes::Bytes>> {
        match self
            .object_store
            .get(&derive_chunk_path(&self.base_path, digest))
            .await
        {
            Ok(res) => {
                let chunk_contents = zstd::stream::decode_all(Cursor::new(res.bytes().await?))?;

                // validate the digest of the remote data to actually match the expected hash
                // before returning.
                if *digest != b3_hash(&chunk_contents) {
                    Err(io::Error::other("chunk contents invalid"))?;
                }

                Ok(Some(chunk_contents.into()))
            }
            Err(object_store::Error::NotFound { .. }) => Ok(None),
            Err(e) => Err(e.into()),
        }
    }

    #[instrument(skip_all, err)]
    async fn put(&self, data: bytes::Bytes) -> io::Result<B3Digest> {
        let digest = b3_hash(&data);

        self.object_store
            .put(&derive_chunk_path(&self.base_path, &digest), data)
            .await?;

        Ok(digest)
    }
}

#[instrument(level=Level::TRACE, skip_all,fields(base_path=%base_path,chunk.digest=%digest),ret(Display))]
fn derive_chunk_path(base_path: &Path, digest: &B3Digest) -> Path {
    base_path
        .child("chunks")
        .child("b3")
        .child(HEXLOWER.encode(&digest.as_slice()[..2]))
        .child(HEXLOWER.encode(digest.as_slice()))
}
