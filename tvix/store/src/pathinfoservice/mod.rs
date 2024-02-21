mod from_addr;
mod grpc;
mod memory;
mod nix_http;
mod sled;
mod utils;

#[cfg(any(feature = "fuse", feature = "virtiofs"))]
mod fs;

use futures::stream::BoxStream;
use nix_compat::nixhash::HashAlgo;
use nix_compat::nixhash::NixHash;
use tonic::async_trait;
use tvix_castore::proto as castorepb;
use tvix_castore::B3Digest;
use tvix_castore::Error;

use crate::proto::PathInfo;

pub use self::from_addr::from_addr;
pub use self::grpc::GRPCPathInfoService;
pub use self::memory::MemoryPathInfoService;
pub use self::nix_http::NixHTTPPathInfoService;
pub use self::sled::SledPathInfoService;

#[cfg(any(feature = "fuse", feature = "virtiofs"))]
pub use self::fs::make_fs;

/// Specifies the hash algo and source to hash.
#[derive(Debug)]
pub enum HashTypeRequest<'a> {
    /// hashes the blob identified by the blake3 digest with the hash algo specified.
    Flat(HashAlgo, &'a B3Digest),
    /// hashes the nar representation of the root node with the hash algo specified.
    /// (and also record its size)
    Nar(HashAlgo, &'a castorepb::node::Node),
}

/// The base trait all PathInfo services need to implement.
#[async_trait]
pub trait PathInfoService: Send + Sync {
    /// Retrieve a PathInfo message by the output digest.
    async fn get(&self, digest: [u8; 20]) -> Result<Option<PathInfo>, Error>;

    /// Store a PathInfo message. Implementations MUST call validate and reject
    /// invalid messages.
    async fn put(&self, path_info: PathInfo) -> Result<PathInfo, Error>;

    /// Provide support to calculate different hashes for store contents.
    /// Receives info on what to hash:
    ///  - in case of a plain file, the hash algo, and the blake3 digest of the blob that needs to be present in the BlobService
    ///  - in case of Nar hashing, a castore root node
    /// Returns the NixHash after doing the hashing, possibly cached.
    /// The second argument returns the size of the NAR, in case of NAR hashing,
    /// and is undefined for plain file hashing.
    async fn calculate_digest(
        &self,
        hash_type_request: &HashTypeRequest,
    ) -> Result<(NixHash, u64), Error>;

    /// Iterate over all PathInfo objects in the store.
    /// Implementations can decide to disallow listing.
    ///
    /// This returns a pinned, boxed stream. The pinning allows for it to be polled easily,
    /// and the box allows different underlying stream implementations to be returned since
    /// Rust doesn't support this as a generic in traits yet. This is the same thing that
    /// [async_trait] generates, but for streams instead of futures.
    fn list(&self) -> BoxStream<'static, Result<PathInfo, Error>>;
}
