use std::io;

use tonic::async_trait;
use tracing::{error, instrument};

use crate::B3Digest;

use super::{BlobService, ChunkMeta};

/// Combinator for a BlobService, using a "local" and "remote" blobservice.
/// Requests are tried in (and returned from) the local store first, only if
/// things are not present there, the remote BlobService is queried.
/// In case the local blobservice doesn't have the blob, we ask the remote
/// blobservice for chunks, and try to read each of these chunks from the local
/// blobservice again, before falling back to the remote one.
/// The remote BlobService is never written to.
pub struct CombinedBlobService<BL, BR> {
    local: BL,
    remote: BR,
}

impl<BL, BR> Clone for CombinedBlobService<BL, BR>
where
    BL: Clone,
    BR: Clone,
{
    fn clone(&self) -> Self {
        Self {
            local: self.local.clone(),
            remote: self.remote.clone(),
        }
    }
}

#[async_trait]
impl<BL, BR> BlobService for CombinedBlobService<BL, BR>
where
    BL: AsRef<dyn BlobService> + Clone + Send + Sync + 'static,
    BR: AsRef<dyn BlobService> + Clone + Send + Sync + 'static,
{
    #[instrument(skip_all, fields(blob.digest=%digest))]
    async fn has(&self, digest: &B3Digest) -> std::io::Result<bool> {
        Ok(self.local.has(digest).await? || self.remote.has(digest).await?)
    }

    #[instrument(skip_all, fields(blob.digest=%digest), err)]
    async fn put(&self, digest: B3Digest, chunks: &[ChunkMeta]) -> io::Result<()> {
        self.remote.put(digest.clone(), chunks).await?;
        self.local.put(digest, chunks).await?;
        Ok(())
    }

    #[instrument(skip_all, fields(blob.digest=%digest), err)]
    async fn chunks(&self, digest: &B3Digest) -> io::Result<Option<Vec<ChunkMeta>>> {
        match self.local.chunks(digest).await {
            Ok(Some(chunks)) => return Ok(Some(chunks)),
            Err(error) => error!(
                ?error,
                "failed to get chunks from local blob service, falling back to remote"
            ),
            _ => {}
        };

        let chunks = self.remote.as_ref().chunks(digest).await?;

        // TODO: Is this correct?
        if let Some(chunks) = &chunks {
            self.local.put(digest.clone(), &chunks).await?;
        }

        Ok(chunks)
    }
}
