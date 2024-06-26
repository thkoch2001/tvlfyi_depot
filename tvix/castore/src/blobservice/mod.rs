use std::io;
use tonic::async_trait;

use crate::proto::stat_blob_response::ChunkMeta as ProtoChunkMeta;
use crate::B3Digest;

mod combinator;
mod from_addr;
mod grpc;
mod memory;
mod object_store;

#[cfg(test)]
pub mod tests;

pub use self::combinator::CombinedBlobService;
pub use self::from_addr::from_addr;
pub use self::grpc::GRPCBlobService;
pub use self::memory::MemoryBlobService;
pub use self::object_store::ObjectStoreBlobService;

#[derive(Debug, Clone)]
pub struct ChunkMeta {
    pub digest: B3Digest,
    pub size: u64,
}

impl Into<ProtoChunkMeta> for ChunkMeta {
    fn into(self) -> ProtoChunkMeta {
        ProtoChunkMeta {
            digest: self.digest.into(),
            size: self.size,
        }
    }
}

impl TryFrom<ProtoChunkMeta> for ChunkMeta {
    type Error = crate::digests::Error;

    fn try_from(value: ProtoChunkMeta) -> Result<Self, Self::Error> {
        Ok(ChunkMeta {
            digest: value.digest.try_into()?,
            size: value.size,
        })
    }
}

/// The base trait all BlobService services need to implement.
/// It provides functions to check whether a given blob exists,
/// a way to read (and seek) a blob, and a method to create a blobwriter handle,
/// which will implement a writer interface, and also provides a close funtion,
/// to finalize a blob and get its digest.
#[async_trait]
pub trait BlobService: Send + Sync {
    /// Check if the service has the blob, by its content hash.
    /// On implementations returning chunks, this must also work for chunks.
    async fn has(&self, digest: &B3Digest) -> io::Result<bool>;

    async fn put(&self, digest: B3Digest, chunks: &[ChunkMeta]) -> io::Result<()>;

    /// Return a list of chunks for a given blob.
    /// TODO: Does this distinction matter anymore?
    /// There's a distinction between returning Ok(None) and Ok(Some(vec![])).
    /// The former return value is sent in case the blob is not present at all,
    /// while the second one is sent in case there's no more granular chunks (or
    /// the backend does not support chunking).
    /// A default implementation checking for existence and then returning it
    /// does not have more granular chunks available is provided.
    async fn chunks(&self, digest: &B3Digest) -> io::Result<Option<Vec<ChunkMeta>>>;

    fn avg_chunk_size(&self) -> u32 {
        256 * 1024
    }
}

#[async_trait]
impl<A> BlobService for A
where
    A: AsRef<dyn BlobService> + Send + Sync,
{
    async fn has(&self, digest: &B3Digest) -> io::Result<bool> {
        self.as_ref().has(digest).await
    }

    async fn put(&self, digest: B3Digest, chunks: &[ChunkMeta]) -> io::Result<()> {
        self.as_ref().put(digest, chunks).await
    }

    async fn chunks(&self, digest: &B3Digest) -> io::Result<Option<Vec<ChunkMeta>>> {
        self.as_ref().chunks(digest).await
    }
}
