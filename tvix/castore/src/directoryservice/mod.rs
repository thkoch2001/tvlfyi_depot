use crate::{proto, B3Digest, Error};
use futures::stream::BoxStream;
use futures::Stream;
use std::pin::Pin;
use std::sync::Arc;
use tonic::async_trait;

mod from_addr;
mod grpc;
mod memory;
mod sled;
mod traverse;
mod utils;

pub use self::from_addr::from_addr;
pub use self::grpc::GRPCDirectoryService;
pub use self::memory::MemoryDirectoryService;
pub use self::sled::SledDirectoryService;
pub use self::traverse::descend_to;

/// The base trait all Directory services need to implement.
/// This is a simple get and put of [crate::proto::Directory], returning their
/// digest.
#[async_trait]
pub trait DirectoryService: Send + Sync {
    /// Looks up a single Directory message by its digest.
    /// The returned Directory message *must* be valid.
    /// In case the directory is not found, Ok(None) is returned.
    ///
    /// It is okay for certain implementations to only allow retrieval of
    /// Directory digests that are at the "root", aka the last element that's
    /// sent to a DirectoryPutter. This makes sense for implementations bundling
    /// closures of directories together in batches.
    async fn get(&self, digest: &B3Digest) -> Result<Option<proto::Directory>, Error>;
    /// Uploads a single Directory message, and returns the calculated
    /// digest, or an error. An error *must* also be returned if the message is
    /// not valid.
    async fn put(&self, directory: proto::Directory) -> Result<B3Digest, Error>;

    /// Looks up a closure of [proto::Directory].
    /// Ideally this would be a `impl Stream<Item = Result<proto::Directory, Error>>`,
    /// and we'd be able to add a default implementation for it here, but
    /// we can't have that yet.
    ///
    /// This returns a pinned, boxed stream. The pinning allows for it to be polled easily,
    /// and the box allows different underlying stream implementations to be returned since
    /// Rust doesn't support this as a generic in traits yet. This is the same thing that
    /// [async_trait] generates, but for streams instead of futures.
    ///
    /// The individual Directory messages *must* be valid.
    fn get_recursive(
        &self,
        root_directory_digest: &B3Digest,
    ) -> BoxStream<Result<proto::Directory, Error>>;

    /// Allows persisting a closure of [proto::Directory], which is a graph of
    /// connected Directory messages.
    fn put_multiple_start(&self) -> Box<dyn DirectoryPutter>;
}

/// Provides a handle to put a closure of connected [proto::Directory] elements.
///
/// The consumer can periodically call [DirectoryPutter::put], starting from the
/// leaves. Once the root is reached, [DirectoryPutter::close] can be called to
/// retrieve the root digest (or an error).
///
/// DirectoryPutters might be created without a single [DirectoryPutter::put],
/// and then dropped without calling [DirectoryPutter::close],
/// for example when ingesting a path that ends up not pointing to a directory,
/// but a single file or symlink.
#[async_trait]
pub trait DirectoryPutter: Send {
    /// Put a individual [proto::Directory] into the store.
    /// Error semantics and behaviour is up to the specific implementation of
    /// this trait.
    /// Due to bursting, the returned error might refer to an object previously
    /// sent via `put`.
    async fn put(&mut self, directory: proto::Directory) -> Result<(), Error>;

    /// Close the stream, and wait for any errors.
    /// If there's been any invalid Directory message uploaded, and error *must*
    /// be returned.
    async fn close(&mut self) -> Result<B3Digest, Error>;
}

#[async_trait]
impl<T: DirectoryService + ?Sized> DirectoryService for Arc<T> {
    async fn get(&self, digest: &B3Digest) -> Result<Option<proto::Directory>, Error> {
        (**self).get(digest).await
    }

    async fn put(&self, directory: proto::Directory) -> Result<B3Digest, Error> {
        (**self).put(directory).await
    }

    fn get_recursive(
        &self,
        root_directory_digest: &B3Digest,
    ) -> Pin<Box<dyn Stream<Item = Result<proto::Directory, Error>> + Send + '_>> {
        (**self).get_recursive(root_directory_digest)
    }

    fn put_multiple_start(&self) -> Box<dyn DirectoryPutter> {
        (**self).put_multiple_start()
    }
}
