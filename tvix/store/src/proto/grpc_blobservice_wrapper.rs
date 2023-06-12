use crate::{
    blobservice::BlobService, proto::sync_read_into_async_read::SyncReadIntoAsyncRead, B3Digest,
};
use std::{
    collections::VecDeque,
    io,
    ops::{Deref, DerefMut},
    pin::Pin,
    sync::Arc,
};
use tokio::task;
use tokio_stream::StreamExt;
use tokio_util::io::ReaderStream;
use tonic::{async_trait, Request, Response, Status, Streaming};
use tracing::{instrument, warn};

pub struct GRPCBlobServiceWrapper {
    blob_service: Arc<dyn BlobService>,
}

impl From<Arc<dyn BlobService>> for GRPCBlobServiceWrapper {
    fn from(value: Arc<dyn BlobService>) -> Self {
        Self {
            blob_service: value,
        }
    }
}

// This is necessary because bytes::BytesMut comes up with
// a default 64 bytes capacity that cannot be changed
// easily if you assume a bytes::BufMut trait implementation
// Therefore, we override the Default implementation here
// TODO(raitobezarius?): upstream me properly
struct BytesMutWithDefaultCapacity<const N: usize> {
    inner: bytes::BytesMut,
}

impl<const N: usize> Deref for BytesMutWithDefaultCapacity<N> {
    type Target = bytes::BytesMut;
    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<const N: usize> DerefMut for BytesMutWithDefaultCapacity<N> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

impl<const N: usize> Default for BytesMutWithDefaultCapacity<N> {
    fn default() -> Self {
        BytesMutWithDefaultCapacity {
            inner: bytes::BytesMut::with_capacity(N),
        }
    }
}

impl<const N: usize> bytes::Buf for BytesMutWithDefaultCapacity<N> {
    fn remaining(&self) -> usize {
        self.inner.remaining()
    }

    fn chunk(&self) -> &[u8] {
        self.inner.chunk()
    }

    fn advance(&mut self, cnt: usize) {
        self.inner.advance(cnt);
    }
}

unsafe impl<const N: usize> bytes::BufMut for BytesMutWithDefaultCapacity<N> {
    fn remaining_mut(&self) -> usize {
        self.inner.remaining_mut()
    }

    unsafe fn advance_mut(&mut self, cnt: usize) {
        self.inner.advance_mut(cnt);
    }

    fn chunk_mut(&mut self) -> &mut bytes::buf::UninitSlice {
        self.inner.chunk_mut()
    }
}

#[async_trait]
impl super::blob_service_server::BlobService for GRPCBlobServiceWrapper {
    // https://github.com/tokio-rs/tokio/issues/2723#issuecomment-1534723933
    type ReadStream =
        Pin<Box<dyn futures::Stream<Item = Result<super::BlobChunk, Status>> + Send + 'static>>;

    #[instrument(skip(self))]
    async fn stat(
        &self,
        request: Request<super::StatBlobRequest>,
    ) -> Result<Response<super::BlobMeta>, Status> {
        let rq = request.into_inner();
        let req_digest = B3Digest::from_vec(rq.digest)
            .map_err(|_e| Status::invalid_argument("invalid digest length"))?;

        if rq.include_chunks || rq.include_bao {
            return Err(Status::internal("not implemented"));
        }

        match self.blob_service.has(&req_digest) {
            Ok(true) => Ok(Response::new(super::BlobMeta::default())),
            Ok(false) => Err(Status::not_found(format!("blob {} not found", &req_digest))),
            Err(e) => Err(e.into()),
        }
    }

    #[instrument(skip(self))]
    async fn read(
        &self,
        request: Request<super::ReadBlobRequest>,
    ) -> Result<Response<Self::ReadStream>, Status> {
        let rq = request.into_inner();

        let req_digest = B3Digest::from_vec(rq.digest)
            .map_err(|_e| Status::invalid_argument("invalid digest length"))?;

        match self.blob_service.open_read(&req_digest) {
            Ok(Some(reader)) => {
                let async_reader: SyncReadIntoAsyncRead<
                    _,
                    BytesMutWithDefaultCapacity<{ 100 * 1024 }>,
                > = reader.into();

                fn stream_mapper(
                    x: Result<bytes::Bytes, io::Error>,
                ) -> Result<super::BlobChunk, Status> {
                    match x {
                        Ok(bytes) => Ok(super::BlobChunk {
                            data: bytes.to_vec(),
                        }),
                        Err(e) => Err(Status::from(e)),
                    }
                }

                let chunks_stream = ReaderStream::new(async_reader).map(stream_mapper);
                Ok(Response::new(Box::pin(chunks_stream)))
            }
            Ok(None) => Err(Status::not_found(format!("blob {} not found", &req_digest))),
            Err(e) => Err(e.into()),
        }
    }

    #[instrument(skip(self))]
    async fn put(
        &self,
        request: Request<Streaming<super::BlobChunk>>,
    ) -> Result<Response<super::PutBlobResponse>, Status> {
        let req_inner = request.into_inner();

        let data_stream = req_inner.map(|x| {
            x.map(|x| VecDeque::from(x.data))
                .map_err(|e| std::io::Error::new(std::io::ErrorKind::InvalidInput, e))
        });

        let data_reader = tokio_util::io::StreamReader::new(data_stream);

        // prepare a writer, which we'll use in the blocking task below.
        let mut writer = self.blob_service.open_write();

        let result = task::spawn_blocking(move || -> Result<super::PutBlobResponse, Status> {
            // construct a sync reader to the data
            let mut reader = tokio_util::io::SyncIoBridge::new(data_reader);

            io::copy(&mut reader, &mut writer).map_err(|e| {
                warn!("error copying: {}", e);
                Status::internal("error copying")
            })?;

            let digest = writer
                .close()
                .map_err(|e| {
                    warn!("error closing stream: {}", e);
                    Status::internal("error closing stream")
                })?
                .to_vec();

            Ok(super::PutBlobResponse { digest })
        })
        .await
        .map_err(|_| Status::internal("failed to wait for task"))??;

        Ok(Response::new(result))
    }
}
