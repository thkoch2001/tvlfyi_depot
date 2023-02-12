pub mod sled;

use crate::Error;

pub use self::sled::SledChunkService;

/// The base trait all ChunkService services need to implement.
/// It allows checking for the existence, download and upload of chunks.
/// It's usually used after consulting a [BlobService] for chunking information.
pub trait ChunkService {
    fn has(&self, digest: &[u8]) -> Result<bool, Error>;

    /// retrieve a chunk by its digest. Implementations MUST validate the digest
    /// matches.
    fn get(&self, digest: &[u8]) -> Result<Option<Vec<u8>>, Error>;
    fn put(&self, data: Vec<u8>) -> Result<Vec<u8>, Error>;
}
