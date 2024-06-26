use std::{io, pin::pin};

use fastcdc::v2020::AsyncStreamCDC;
use pin_project_lite::pin_project;
use tokio::{
    io::{AsyncWriteExt, DuplexStream},
    task::JoinHandle,
};
use tokio_stream::StreamExt;
use tracing::instrument;

use crate::{
    blobservice::{BlobService, ChunkMeta},
    chunkstore::ChunkStore,
    B3Digest, B3HashingReader,
};

pin_project! {
    /// Takes care of blob uploads.
    /// All writes are relayed to self.writer, and we continuously poll the
    /// future (which will internally read from the other side of the pipe and
    /// upload chunks).
    /// Our BlobWriter::close() needs to drop self.writer, so the other side
    /// will read EOF and can finalize the blob.
    /// The future should then resolve and return the blob digest.
    pub struct BlobWriter {
        #[pin]
        writer: Option<DuplexStream>,

        task: Option<JoinHandle<io::Result<B3Digest>>>,

        fut_output: Option<io::Result<B3Digest>>
    }
}

impl BlobWriter {
    pub fn new<BS, CS>(blob_service: BS, chunk_store: CS) -> Self
    where
        BS: BlobService + Clone + 'static,
        CS: ChunkStore + Clone + 'static,
    {
        let avg_chunk_size = blob_service.avg_chunk_size();
        let (w, r) = tokio::io::duplex(avg_chunk_size as usize * 10);

        let task = tokio::spawn(chunk_and_upload(
            r,
            chunk_store,
            blob_service,
            avg_chunk_size / 2,
            avg_chunk_size,
            avg_chunk_size * 2,
        ));

        BlobWriter {
            writer: Some(w),
            task: Some(task),
            fut_output: None,
        }
    }

    pub async fn close(&mut self) -> io::Result<B3Digest> {
        match self.writer.take() {
            Some(mut writer) => {
                // shut down the writer, so the other side will read EOF.
                writer.shutdown().await?;

                // take out the future.
                let fut = self.task.take().expect("fut must be some");
                // await it.
                let resp = pin!(fut).await.expect("tokio join error");

                match resp.as_ref() {
                    // In the case of an Ok value, we store it in self.fut_output,
                    // so future calls to close can return that.
                    Ok(b3_digest) => {
                        self.fut_output = Some(Ok(b3_digest.clone()));
                    }
                    Err(e) => {
                        // for the error type, we need to cheat a bit, as
                        // they're not clone-able.
                        // Simply store a sloppy clone, with the same ErrorKind and message there.
                        self.fut_output = Some(Err(std::io::Error::new(e.kind(), e.to_string())))
                    }
                }
                resp
            }
            None => {
                // called a second time, return self.fut_output.
                match self.fut_output.as_ref().unwrap() {
                    Ok(ref b3_digest) => Ok(b3_digest.clone()),
                    Err(e) => Err(std::io::Error::new(e.kind(), e.to_string())),
                }
            }
        }
    }
}

/// Reads blob contents from a AsyncRead, chunks and uploads them.
/// On success, returns a [StatBlobResponse] pointing to the individual chunks.
#[instrument(skip_all, fields(min_chunk_size, avg_chunk_size, max_chunk_size), err)]
async fn chunk_and_upload<CS, BS>(
    r: DuplexStream,
    chunk_store: CS,
    blob_service: BS,
    min_chunk_size: u32,
    avg_chunk_size: u32,
    max_chunk_size: u32,
) -> io::Result<B3Digest>
where
    BS: BlobService,
    CS: ChunkStore + Clone + 'static,
{
    // wrap reader with something calculating the blake3 hash of all data read.
    let mut b3_r = B3HashingReader::from(r);
    // set up a fastcdc chunker
    let mut chunker =
        AsyncStreamCDC::new(&mut b3_r, min_chunk_size, avg_chunk_size, max_chunk_size);

    /// This really should just belong into the closure at
    /// `chunker.as_stream().then(|_| { … })``, but if we try to, rustc spits
    /// higher-ranked lifetime errors at us.
    async fn fastcdc_chunk_uploader<CS>(
        resp: Result<fastcdc::v2020::ChunkData, fastcdc::v2020::Error>,
        chunk_store: CS,
    ) -> std::io::Result<ChunkMeta>
    where
        CS: ChunkStore,
    {
        let chunk_data = resp?;
        let digest: B3Digest = blake3::hash(&chunk_data.data).as_bytes().into();

        upload_chunk(chunk_store, digest, chunk_data.data).await
    }

    // Use the fastcdc chunker to produce a stream of chunks, and upload these
    // that don't exist to the backend.
    let chunks = chunker
        .as_stream()
        .then(|resp| fastcdc_chunk_uploader(resp, chunk_store.clone()))
        .collect::<io::Result<Vec<ChunkMeta>>>()
        .await?;

    // check for Blob, if it doesn't exist, persist.
    let blob_digest: B3Digest = b3_r.digest().into();
    if !blob_service.has(&blob_digest).await? {
        blob_service.put(blob_digest.clone(), &chunks).await?;
    }

    Ok(blob_digest)
}

/// upload chunk if it doesn't exist yet.
#[instrument(skip_all, fields(chunk.digest = %digest, chunk.size = data.len()), err)]
async fn upload_chunk<CS>(
    chunk_store: CS,
    digest: B3Digest,
    data: Vec<u8>,
) -> std::io::Result<ChunkMeta>
where
    CS: ChunkStore,
{
    let exists = chunk_store.has(&digest).await?;
    let size = data.len() as u64;

    if !exists {
        let put_digest = chunk_store.put(data.into()).await?;
        debug_assert_eq!(
            digest, put_digest,
            "tvix bug: unexpected chunk digest mismatch"
        );
    }

    Ok(ChunkMeta {
        digest: digest.into(),
        size,
    })
}

impl tokio::io::AsyncWrite for BlobWriter {
    fn poll_write(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        buf: &[u8],
    ) -> std::task::Poll<Result<usize, io::Error>> {
        let this = self.project();

        // write to the underlying writer
        this.writer
            .as_pin_mut()
            .expect("writer must be some")
            .poll_write(cx, buf)
    }

    fn poll_flush(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Result<(), io::Error>> {
        let this = self.project();

        // Call poll_flush on the writer
        this.writer
            .as_pin_mut()
            .expect("writer must be some")
            .poll_flush(cx)
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

#[cfg(test)]
mod test {
    use tokio::io::AsyncWriteExt;

    use crate::{
        blob::BlobWriter,
        blobservice::{BlobService, MemoryBlobService},
        chunkstore::{ChunkStore, MemoryChunkStore},
        fixtures::{BLOB_A, BLOB_A_DIGEST},
    };
    use std::sync::Arc;

    #[tokio::test]
    async fn test_blob_write() {
        let blob_service = Arc::new(MemoryBlobService::default()) as Arc<dyn BlobService>;
        let chunk_store = Arc::new(MemoryChunkStore::default()) as Arc<dyn ChunkStore>;

        let blob_writer = BlobWriter::new(blob_service, chunk_store);
        blob_writer
            .write_all(&BLOB_A)
            .await
            .expect("blob write succeeds");

        let blob_digest = blob_writer.close().await.expect("blob close succeeds");

        assert_eq!(BLOB_A_DIGEST.clone(), blob_digest);

        // Now we should have the blob
        assert!(blob_service.has(&BLOB_A_DIGEST).await.unwrap());
    }
}
