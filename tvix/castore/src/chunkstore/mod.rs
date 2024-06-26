use std::{collections::HashMap, io, sync::Arc};

use parking_lot::RwLock;
use tonic::async_trait;
use tracing::instrument;

use crate::B3Digest;

mod grpc;
pub use grpc::GRPC;

mod object_store;
pub use object_store::ObjectStore;

/// A [ChunkStore] stores individual (small) chunks of data.
/// It is usually consulted after asking a [crate::BlobService] for
/// chunking information.
#[async_trait]
pub trait ChunkStore: Send + Sync {
    /// Check if the chunk with the given content digest exists.
    async fn has(&self, digest: &B3Digest) -> io::Result<bool>;

    /// Get the chunk with the given content digest.
    async fn get(&self, digest: &B3Digest) -> io::Result<Option<bytes::Bytes>>;

    /// Insert the chunk, returning its content digest.
    async fn put(&self, data: bytes::Bytes) -> io::Result<B3Digest>;
}

/// A in-memory implementation of [ChunkStore], storing chunks in a HashMap,
/// keyed by their blake3 digest.
#[derive(Clone, Default)]
pub struct Memory {
    db: Arc<RwLock<HashMap<B3Digest, bytes::Bytes>>>,
}

#[async_trait]
impl ChunkStore for Memory {
    #[instrument(skip_all, err, fields(chunk.digest=%digest))]
    async fn has(&self, digest: &B3Digest) -> io::Result<bool> {
        Ok(self.db.read().contains_key(digest))
    }

    #[instrument(skip_all, err, fields(chunk.digest=%digest))]
    async fn get(&self, digest: &B3Digest) -> io::Result<Option<bytes::Bytes>> {
        Ok(self.db.read().get(digest).cloned())
    }

    #[instrument(skip_all, err)]
    async fn put(&self, data: bytes::Bytes) -> io::Result<B3Digest> {
        let digest = b3_hash(&data);
        self.db.write().insert(digest.clone(), data);
        Ok(digest)
    }
}

#[async_trait]
impl<A> ChunkStore for A
where
    A: AsRef<dyn ChunkStore> + Send + Sync,
{
    async fn has(&self, digest: &B3Digest) -> io::Result<bool> {
        self.as_ref().has(digest).await
    }

    async fn get(&self, digest: &B3Digest) -> io::Result<Option<bytes::Bytes>> {
        self.as_ref().get(digest).await
    }

    async fn put(&self, data: bytes::Bytes) -> io::Result<B3Digest> {
        self.as_ref().put(data).await
    }
}

/// Invoke the BLAKE3 hash function on some bytes in memory.
/// Use rayon for hashing in case the data is larger than the rule-of-thumb
/// threshold.
fn b3_hash(data: impl AsRef<[u8]>) -> B3Digest {
    let data = data.as_ref();
    if data.len() > 128 * 1024 {
        let mut hash = blake3::Hasher::new();
        hash.update_rayon(&data);
        hash.finalize().as_bytes().into()
    } else {
        blake3::hash(&data).as_bytes().into()
    }
}
