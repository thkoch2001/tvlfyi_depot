use crate::{proto, Error};

mod sled;

pub use self::sled::SledBlobService;

/// The base trait all BlobService services need to implement.
/// As it provides only information about how blobs are chunked,
/// a proper API also requires a [ChunkService] as well.
pub trait BlobService {
    fn stat(&self, req: &proto::StatBlobRequest) -> Result<Option<proto::BlobMeta>, Error>;
    fn put(&self, blob_digest: &[u8], blob_meta: proto::BlobMeta) -> Result<(), Error>;
}
