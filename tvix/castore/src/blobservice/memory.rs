use parking_lot::RwLock;
use std::io::{self};
use std::{collections::HashMap, sync::Arc};
use tonic::async_trait;
use tracing::instrument;

use super::{BlobService, ChunkMeta};
use crate::B3Digest;

#[derive(Clone, Default)]
pub struct MemoryBlobService {
    db: Arc<RwLock<HashMap<B3Digest, Vec<ChunkMeta>>>>,
}

#[async_trait]
impl BlobService for MemoryBlobService {
    #[instrument(skip_all, ret, err, fields(blob.digest=%digest))]
    async fn has(&self, digest: &B3Digest) -> io::Result<bool> {
        let db = self.db.read();
        Ok(db.contains_key(digest))
    }

    #[instrument(skip_all, ret, err, fields(blob.digest=%digest))]
    async fn put(&self, digest: B3Digest, chunks: &[ChunkMeta]) -> io::Result<()> {
        self.db.write().insert(digest, chunks.to_vec());
        Ok(())
    }

    #[instrument(skip_all, ret, err, fields(blob.digest=%digest))]
    async fn chunks(&self, digest: &B3Digest) -> io::Result<Option<Vec<ChunkMeta>>> {
        Ok(self.db.read().get(digest).cloned())
    }
}

pub struct MemoryBlobWriter {
    db: Arc<RwLock<HashMap<B3Digest, Vec<u8>>>>,

    /// Contains the buffer Vec and hasher, or None if already closed
    writers: Option<(Vec<u8>, blake3::Hasher)>,

    /// The digest that has been returned, if we successfully closed.
    digest: Option<B3Digest>,
}
