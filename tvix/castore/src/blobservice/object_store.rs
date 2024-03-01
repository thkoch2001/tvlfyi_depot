use std::{
    io::{self, Cursor},
    sync::Arc,
    task::Poll,
};

use data_encoding::HEXLOWER;
use fastcdc::v2020::AsyncStreamCDC;
use futures::Future;
use object_store::{path::Path, ObjectStore};
use pin_project_lite::pin_project;
use prost::Message;
use tokio::io::{AsyncRead, AsyncWrite};
use tokio_stream::StreamExt;
use tonic::async_trait;
use tracing::instrument;
use url::Url;

use crate::{
    proto::{stat_blob_response::ChunkMeta, StatBlobResponse},
    B3Digest,
};

use super::{BlobReader, BlobService, BlobWriter};

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
    /// Any path suffix becomes base path.
    pub fn from_url(url: &Url) -> Result<Self, object_store::Error> {
        let (object_store, path) = object_store::parse_url(url)?;

        Ok(Self {
            object_store: Arc::new(object_store),
            base_path: path,
            avg_chunk_size: 256 * 1024,
        })
    }
}

fn derive_blob_path(base_path: &Path, digest: &B3Digest) -> Path {
    base_path
        .child("blobs")
        .child("b3")
        .child(HEXLOWER.encode(&digest.as_slice()[..2]))
        .child(HEXLOWER.encode(digest.as_slice()))
}

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
            Err(object_store::Error::NotFound { .. }) => Ok(false),
            Err(e) => Err(e)?,
        }
    }

    #[instrument(skip_all, err, fields(blob.digest=%digest))]
    async fn open_read(&self, digest: &B3Digest) -> io::Result<Option<Box<dyn BlobReader>>> {
        let p = derive_chunk_path(&self.base_path, digest);

        Ok(match self.object_store.get(&p).await {
            Ok(res) => {
                // fetch the entire chunk into memory, and return a io::Cursor over it.
                // FUTUREWORK: should we use into_stream()?
                let chunk_bytes = res.bytes().await?;
                Some(Box::new(Cursor::new(chunk_bytes)))
            }
            Err(object_store::Error::NotFound { .. }) => {
                // if the chunk was not found, stop here.
                // clients should fetch granuarly.
                // TODO: make this configurable, for clients talking to S3
                // directly it should be possible?
                // TODO: clarify behaviour for server impls, and explicitly
                // document in the proto docs.
                None
            }
            Err(e) => Err(e)?,
        })
    }

    #[instrument(skip_all)]
    async fn open_write(&self) -> Box<dyn BlobWriter> {
        // ObjectStoreBlobWriter implements AsyncWrite, but all the chunking
        // needs an AsyncRead, so we create a pipe here.
        // In its `AsyncWrite` implementation, `ObjectStoreBlobWriter` delegates
        // writes to w. It periodically polls the future that's reading from the
        // other side.
        let (w, r) = tokio::io::duplex(self.avg_chunk_size as usize * 10);

        Box::new(ObjectStoreBlobWriter {
            writer: w,
            fut: Some(Box::new(chunk_and_upload(
                r,
                self.object_store.clone(),
                self.base_path.clone(),
                self.avg_chunk_size / 2,
                self.avg_chunk_size,
                self.avg_chunk_size * 2,
            ))),
        })
    }

    #[instrument(skip_all, err, fields(blob.digest=%digest))]
    async fn chunks(&self, digest: &B3Digest) -> io::Result<Option<Vec<ChunkMeta>>> {
        let p = derive_blob_path(&self.base_path, digest);

        Ok(match self.object_store.get(&p).await {
            Ok(get_result) => {
                // fetch the data at the blob path
                let blob_data = get_result.bytes().await?;
                // parse into StatBlobResponse
                let stat_blob_response: StatBlobResponse = StatBlobResponse::decode(blob_data)?;

                Some(stat_blob_response.chunks)
            }
            Err(object_store::Error::NotFound { .. }) => None,
            Err(err) => Err(err)?,
        })
    }
}

/// Reads blob contents from a AsyncRead, chunks and uploads them.
/// On success, returns a [StatBlobResponse] pointing to the individual chunks.
async fn chunk_and_upload<R: AsyncRead + Unpin>(
    r: R,
    object_store: Arc<dyn ObjectStore>,
    base_path: Path,
    min_chunk_size: u32,
    avg_chunk_size: u32,
    max_chunk_size: u32,
) -> io::Result<B3Digest> {
    // wrap reader with something calculating the blake3 hash of all data read.
    let mut b3_r = B3Reader::from(r);
    // set up a fastcdc chunker
    let mut chunker =
        AsyncStreamCDC::new(&mut b3_r, min_chunk_size, avg_chunk_size, max_chunk_size);

    /// upload chunks if they don't exist yet.
    async fn upload_chunk(
        e: Result<fastcdc::v2020::ChunkData, fastcdc::v2020::Error>,
        base_path: Path,
        object_store: Arc<dyn ObjectStore>,
    ) -> std::io::Result<ChunkMeta> {
        match e {
            // error while reading raw data in chunker
            Err(err) => Err(err)?,
            Ok(chunk_data) => {
                // calculate the b3 digest of that chunk.
                let chunk_digest: B3Digest = blake3::hash(&chunk_data.data).as_bytes().into();

                // check if chunk already exists, else upload
                let chunk_path = derive_chunk_path(&base_path, &chunk_digest);
                match object_store.head(&chunk_path).await {
                    // chunk already exists, nothing to do
                    Ok(_) => {}
                    // chunk does not yet exist, upload first
                    Err(object_store::Error::NotFound { .. }) => {
                        object_store
                            .as_ref()
                            .put(&chunk_path, chunk_data.data.into())
                            .await?;
                    }
                    // other error
                    Err(err) => Err(err)?,
                }

                Ok(ChunkMeta {
                    digest: chunk_digest.into(),
                    size: chunk_data.length as u64,
                })
            }
        }
    }

    // use it to produce a stream of chunks.
    let chunks = chunker
        .as_stream()
        .then(|e| {
            let base_path = base_path.clone();
            let object_store = object_store.clone();
            upload_chunk(e, base_path, object_store)
        })
        .collect::<io::Result<Vec<ChunkMeta>>>()
        .await?;

    let stat_blob_response = StatBlobResponse {
        chunks,
        bao: "".into(), // still todo
    };

    // check for Blob, if it doesn't exist, persist.
    let blob_digest: B3Digest = b3_r.digest().as_bytes().into();
    let blob_path = derive_blob_path(&base_path, &blob_digest);

    match object_store.head(&blob_path).await {
        // blob already exists, nothing to do
        Ok(_) => {}
        // chunk does not yet exist, upload first
        Err(object_store::Error::NotFound { .. }) => {
            object_store
                .put(&blob_path, stat_blob_response.encode_to_vec().into())
                .await?;
        }
        Err(err) => {
            // other error
            Err(err)?
        }
    }

    Ok(blob_digest)
}

pin_project! {
    struct B3Reader<R>
    where
        R: AsyncRead,
    {
        #[pin]
        inner: R,
        hasher: blake3::Hasher,
    }
}

impl<R> B3Reader<R>
where
    R: AsyncRead,
{
    pub fn from(r: R) -> Self {
        Self {
            inner: r,
            hasher: blake3::Hasher::new(),
        }
    }

    /// Return the digest.
    pub fn digest(self) -> blake3::Hash {
        self.hasher.finalize()
    }
}

impl<R> tokio::io::AsyncRead for B3Reader<R>
where
    R: AsyncRead,
{
    fn poll_read(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        buf: &mut tokio::io::ReadBuf<'_>,
    ) -> Poll<io::Result<()>> {
        let buf_filled_len_before = buf.filled().len();

        let this = self.project();
        let ret = this.inner.poll_read(cx, buf);

        // write everything new filled into the hasher.
        this.hasher.update(&buf.filled()[buf_filled_len_before..]);

        ret
    }
}

pin_project! {
    /// Takes care of blob uploads.
    /// All writes are relayed to self.writer, and we continuously poll the future
    /// (which will internally read from the other side of the pipe).
    /// Our close() needs to keep polling it until everything has been processed
    /// (and uploaded), then we can return the blob digest.
    pub struct ObjectStoreBlobWriter<W>
    where W: AsyncWrite,
    {
        #[pin]
        writer: W,

        #[pin]
        fut: Option< Box<dyn Future<Output = std::io::Result<B3Digest>> + Send>>,
    }
}

impl<W> tokio::io::AsyncWrite for ObjectStoreBlobWriter<W>
where
    W: AsyncWrite + Send + Unpin,
{
    fn poll_write(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        buf: &[u8],
    ) -> std::task::Poll<Result<usize, io::Error>> {
        // TODO: poll

        let this = self.project();
        this.writer.poll_write(cx, buf)
    }

    fn poll_flush(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Result<(), io::Error>> {
        // TODO: poll

        let this = self.project();
        this.writer.poll_flush(cx)
    }

    fn poll_shutdown(
        self: std::pin::Pin<&mut Self>,
        _cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Result<(), io::Error>> {
        // There's nothing to do on shutdown. We might have written some chunks
        // that are nowhere else referenced, but cleaning them up here would be racy.
        std::task::Poll::Ready(Ok(()))
    }
}

#[async_trait]
impl<W> BlobWriter for ObjectStoreBlobWriter<W>
where
    W: AsyncWrite + Send + Unpin,
{
    async fn close(&mut self) -> io::Result<B3Digest> {
        // poll the future until completion.
        match self.fut.take() {
            Some(fut) => {
                let fut = Box::into_pin(fut);
                fut.await
            }
            None => Err(std::io::Error::new(
                io::ErrorKind::BrokenPipe,
                "already closed",
            )),
        }
    }
}

#[cfg(test)]
mod test {
    use std::{io::Cursor, sync::Arc};

    use url::Url;

    use super::chunk_and_upload;
    use crate::{
        blobservice::{BlobService, ObjectStoreBlobService},
        fixtures::{BLOB_A, BLOB_A_DIGEST},
    };

    #[tokio::test]
    async fn upload_blob_a() {
        let blobsvc = ObjectStoreBlobService::from_url(&Url::parse("memory:///").unwrap()).unwrap();

        assert!(!blobsvc.has(&BLOB_A_DIGEST).await.unwrap());

        // upload blob a
        let mut bw = blobsvc.open_write().await;
        tokio::io::copy(&mut Cursor::new(BLOB_A.to_vec()), &mut bw)
            .await
            .expect("copy succeeds");

        assert!(blobsvc.has(&BLOB_A_DIGEST).await.unwrap());
    }

    /// Tests chunk_and_upload directly, bypassing the BlobWriter at open_write().
    #[tokio::test]
    async fn test_chunk_and_upload() {
        let blobsvc =
            Arc::new(ObjectStoreBlobService::from_url(&Url::parse("memory:///").unwrap()).unwrap());

        let blob_digest = chunk_and_upload(
            &mut Cursor::new(BLOB_A.to_vec()),
            blobsvc.object_store.clone(),
            object_store::path::Path::from("/"),
            1024 / 2,
            1024,
            1024 * 2,
        )
        .await
        .expect("chunk_and_upload succeeds");

        assert_eq!(BLOB_A_DIGEST.clone(), blob_digest);

        // Now we should have the blob
        assert!(blobsvc.has(&BLOB_A_DIGEST).await.unwrap());
    }
}
