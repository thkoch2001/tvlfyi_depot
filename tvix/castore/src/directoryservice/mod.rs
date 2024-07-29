use crate::composition::{Registry, ServiceBuilder};
use crate::{B3Digest, Error};
use crate::ValidateNodeError;
use std::{iter::Peekable, collections::HashSet};

use futures::stream::BoxStream;
use tonic::async_trait;
mod combinators;
mod directory_graph;
mod from_addr;
mod grpc;
mod memory;
mod object_store;
mod order_validator;
mod simple_putter;
mod sled;
#[cfg(test)]
pub mod tests;
mod traverse;
mod utils;

pub use self::combinators::{Cache, CacheConfig};
pub use self::directory_graph::DirectoryGraph;
pub use self::from_addr::from_addr;
pub use self::grpc::{GRPCDirectoryService, GRPCDirectoryServiceConfig};
pub use self::memory::{MemoryDirectoryService, MemoryDirectoryServiceConfig};
pub use self::object_store::{ObjectStoreDirectoryService, ObjectStoreDirectoryServiceConfig};
pub use self::order_validator::{LeavesToRootValidator, OrderValidator, RootToLeavesValidator};
pub use self::simple_putter::SimplePutter;
pub use self::sled::{SledDirectoryService, SledDirectoryServiceConfig};
pub use self::traverse::descend_to;
pub use self::utils::traverse_directory;

#[cfg(feature = "cloud")]
mod bigtable;

#[cfg(feature = "cloud")]
pub use self::bigtable::{BigtableDirectoryService, BigtableParameters};

/// The base trait all Directory services need to implement.
/// This is a simple get and put of [Directory], returning their
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
    async fn get(&self, digest: &B3Digest) -> Result<Option<Directory>, Error>;
    /// Uploads a single Directory message, and returns the calculated
    /// digest, or an error. An error *must* also be returned if the message is
    /// not valid.
    async fn put(&self, directory: Directory) -> Result<B3Digest, Error>;

    /// Looks up a closure of [Directory].
    /// Ideally this would be a `impl Stream<Item = Result<Directory, Error>>`,
    /// and we'd be able to add a default implementation for it here, but
    /// we can't have that yet.
    ///
    /// This returns a pinned, boxed stream. The pinning allows for it to be polled easily,
    /// and the box allows different underlying stream implementations to be returned since
    /// Rust doesn't support this as a generic in traits yet. This is the same thing that
    /// [async_trait] generates, but for streams instead of futures.
    ///
    /// The individually returned Directory messages *must* be valid.
    /// Directories are sent in an order from the root to the leaves, so that
    /// the receiving side can validate each message to be a connected to the root
    /// that has initially been requested.
    ///
    /// In case the directory can not be found, this should return an empty stream.
    fn get_recursive(
        &self,
        root_directory_digest: &B3Digest,
    ) -> BoxStream<'static, Result<Directory, Error>>;

    /// Allows persisting a closure of [Directory], which is a graph of
    /// connected Directory messages.
    fn put_multiple_start(&self) -> Box<dyn DirectoryPutter>;
}

#[async_trait]
impl<A> DirectoryService for A
where
    A: AsRef<dyn DirectoryService> + Send + Sync,
{
    async fn get(&self, digest: &B3Digest) -> Result<Option<Directory>, Error> {
        self.as_ref().get(digest).await
    }

    async fn put(&self, directory: Directory) -> Result<B3Digest, Error> {
        self.as_ref().put(directory).await
    }

    fn get_recursive(
        &self,
        root_directory_digest: &B3Digest,
    ) -> BoxStream<'static, Result<Directory, Error>> {
        self.as_ref().get_recursive(root_directory_digest)
    }

    fn put_multiple_start(&self) -> Box<dyn DirectoryPutter> {
        self.as_ref().put_multiple_start()
    }
}

/// Provides a handle to put a closure of connected [Directory] elements.
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
    /// Put a individual [Directory] into the store.
    /// Error semantics and behaviour is up to the specific implementation of
    /// this trait.
    /// Due to bursting, the returned error might refer to an object previously
    /// sent via `put`.
    async fn put(&mut self, directory: Directory) -> Result<(), Error>;

    /// Close the stream, and wait for any errors.
    /// If there's been any invalid Directory message uploaded, and error *must*
    /// be returned.
    async fn close(&mut self) -> Result<B3Digest, Error>;
}

/// Registers the builtin DirectoryService implementations with the registry
pub(crate) fn register_directory_services(reg: &mut Registry) {
    reg.register::<Box<dyn ServiceBuilder<Output = dyn DirectoryService>>, super::directoryservice::ObjectStoreDirectoryServiceConfig>("objectstore");
    reg.register::<Box<dyn ServiceBuilder<Output = dyn DirectoryService>>, super::directoryservice::MemoryDirectoryServiceConfig>("memory");
    reg.register::<Box<dyn ServiceBuilder<Output = dyn DirectoryService>>, super::directoryservice::CacheConfig>("cache");
    reg.register::<Box<dyn ServiceBuilder<Output = dyn DirectoryService>>, super::directoryservice::GRPCDirectoryServiceConfig>("grpc");
    reg.register::<Box<dyn ServiceBuilder<Output = dyn DirectoryService>>, super::directoryservice::SledDirectoryServiceConfig>("sled");
    #[cfg(feature = "cloud")]
    {
        reg.register::<Box<dyn ServiceBuilder<Output = dyn DirectoryService>>, super::directoryservice::BigtableParameters>("bigtable");
    }
}

// Note: keep the docstrings in sync with the proto definitions
/// A Directory can contain Directory, File or Symlink nodes.
/// Each of these nodes have a name attribute, which is the basename in that
/// directory and node type specific attributes.
/// The name attribute:
///  - MUST not contain slashes or null bytes
///  - MUST not be '.' or '..'
///  - MUST be unique across all three lists
/// Elements in each list need to be lexicographically ordered by the name
/// attribute.
#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct Directory {
    pub directories: Vec<DirectoryNode>,
    pub files: Vec<FileNode>,
    pub symlinks: Vec<SymlinkNode>,
}

/// Checks a Node name for validity as an intermediate node.
/// We disallow slashes, null bytes, '.', '..' and the empty string.
pub(crate) fn validate_node_name(name: &[u8]) -> Result<(), ValidateNodeError> {
    if name.is_empty()
        || name == b".."
        || name == b"."
        || name.contains(&0x00)
        || name.contains(&b'/')
    {
        Err(ValidateNodeError::InvalidName(name.to_owned()))
    } else {
        Ok(())
    }
}

/// A DirectoryNode represents a directory in a Directory.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DirectoryNode {
    /// The (base)name of the directory
    name: Vec<u8>,
    /// The blake3 hash of a Directory message, serialized in protobuf canonical form.
    digest: B3Digest,
    /// Number of child elements in the Directory referred to by `digest`.
    /// Calculated by summing up the numbers of `directories`, `files` and
    /// `symlinks`, and for each directory, its size field. Used for inode number
    /// calculation.
    /// This field is precisely as verifiable as any other Merkle tree edge.
    /// Resolve `digest`, and you can compute it incrementally. Resolve the entire
    /// tree, and you can fully compute it from scratch.
    /// A credulous implementation won't reject an excessive size, but this is
    /// harmless: you'll have some ordinals without nodes. Undersizing is obvious
    /// and easy to reject: you won't have an ordinal for some nodes.
    size: u64,
}

impl DirectoryNode {
    pub fn new(name: Vec<u8>, digest: B3Digest, size: u64) -> Result<Self, ValidateNodeError> {
        validate_node_name(&name)?;
        Ok(Self { name, digest, size })
    }
}


/// A FileNode represents a regular or executable file in a Directory.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FileNode {
    /// The (base)name of the file
    name: Vec<u8>,

    /// The blake3 digest of the file contents
    digest: B3Digest,

    /// The file content size
    size: u64,
  
    /// Whether the file is executable
    executable: bool,
}

impl FileNode {
    pub fn new(name: Vec<u8>, digest: B3Digest, size: u64, executable: bool) -> Result<Self, ValidateNodeError> {
        validate_node_name(&name)?;
        Ok(Self { name, digest, size, executable })
    }
}

/// A SymlinkNode represents a symbolic link in a Directory.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SymlinkNode {
    /// The (base)name of the symlink
    name: Vec<u8>,
    /// The target of the symlink.
    target: Vec<u8>,
}

impl SymlinkNode {
    pub fn new(name: Vec<u8>, target: Vec<u8>) -> Result<Self, ValidateNodeError> {
        if target.is_empty() || target.contains(&b'\0') {
            return Err(ValidateNodeError::InvalidSymlinkTarget(target));
        }
        validate_node_name(&name)?;
        Ok(Self { name, target })
    }
}

/// A Node is either a DirectoryNode, FileNode or SymlinkNode.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Node {
    Directory(DirectoryNode),
    File(FileNode),
    Symlink(SymlinkNode),
}

/// NamedNode is implemented for [FileNode], [DirectoryNode] and [SymlinkNode]
/// and [Node], so we can ask all of them for the name easily.
pub trait NamedNode {
    fn get_name(&self) -> &[u8];
}

impl NamedNode for &FileNode {
    fn get_name(&self) -> &[u8] {
        &self.name
    }
}

impl NamedNode for &DirectoryNode {
    fn get_name(&self) -> &[u8] {
        &self.name
    }
}

impl NamedNode for &SymlinkNode {
    fn get_name(&self) -> &[u8] {
        &self.name
    }
}

impl NamedNode for Node {
    fn get_name(&self) -> &[u8] {
        match self {
            Node::File(node_file) => &node_file.name,
            Node::Directory(node_directory) => &node_directory.name,
            Node::Symlink(node_symlink) => &node_symlink.name,
        }
    }
}

impl Node {
    /// Returns the node with a new name.
    pub fn rename(self, name: bytes::Bytes) -> Self {
        match self {
            Node::Directory(n) => Node::Directory(DirectoryNode { name, ..n }),
            Node::File(n) => Node::File(FileNode { name, ..n }),
            Node::Symlink(n) => Node::Symlink(SymlinkNode { name, ..n }),
        }
    }
}

impl PartialOrd for FileNode {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for FileNode {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.get_name().cmp(other.get_name())
    }
}

impl PartialOrd for DirectoryNode {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for DirectoryNode {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.get_name().cmp(other.get_name())
    }
}

impl PartialOrd for SymlinkNode {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for SymlinkNode {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.get_name().cmp(other.get_name())
    }
}

fn checked_sum(iter: impl IntoIterator<Item = u64>) -> Option<u64> {
    iter.into_iter().try_fold(0u64, |acc, i| acc.checked_add(i))
}

impl Directory {
    /// The size of a directory is the number of all regular and symlink elements,
    /// the number of directory elements, and their size fields.
    pub fn size(&self) -> u64 {
        if cfg!(debug_assertions) {
            self.size_checked()
                .expect("Directory::size exceeds u64::MAX")
        } else {
            self.size_checked().unwrap_or(u64::MAX)
        }
    }

    fn size_checked(&self) -> Option<u64> {
        checked_sum([
            self.files.len().try_into().ok()?,
            self.symlinks.len().try_into().ok()?,
            self.directories.len().try_into().ok()?,
            checked_sum(self.directories.iter().map(|e| e.size))?,
        ])
    }

    /// Calculates the digest of a Directory, which is the blake3 hash of a
    /// Directory protobuf message, serialized in protobuf canonical form.
    pub fn digest(&self) -> B3Digest {
        let mut hasher = blake3::Hasher::new();

        hasher
            .update(&self.encode_to_vec())
            .finalize()
            .as_bytes()
            .into()
    }

    /// Allows iterating over all three nodes ([DirectoryNode], [FileNode],
    /// [SymlinkNode]) in an ordered fashion, as long as the individual lists
    /// are sorted (which can be checked by the [Directory::validate]).
    pub fn nodes(&self) -> DirectoryNodesIterator {
        return DirectoryNodesIterator {
            i_directories: self.directories.iter().peekable(),
            i_files: self.files.iter().peekable(),
            i_symlinks: self.symlinks.iter().peekable(),
        };
    }

    /// Adds the specified [Node] to the [Directory], preserving sorted entries.
    /// This assumes the [Directory] to be sorted prior to adding the node.
    ///
    /// Inserting an element that already exists with the same name in the directory is not
    /// supported.
    pub fn add(&mut self, node: Node) {
        debug_assert!(
            !self.files.iter().any(|x| x.get_name() == node.get_name()),
            "name already exists in files"
        );
        debug_assert!(
            !self
                .directories
                .iter()
                .any(|x| x.get_name() == node.get_name()),
            "name already exists in directories"
        );
        debug_assert!(
            !self
                .symlinks
                .iter()
                .any(|x| x.get_name() == node.get_name()),
            "name already exists in symlinks"
        );

        match node {
            Node::File(node) => {
                let pos = self
                    .files
                    .binary_search(&node)
                    .expect_err("Tvix bug: dir entry with name already exists");
                self.files.insert(pos, node);
            }
            Node::Directory(node) => {
                let pos = self
                    .directories
                    .binary_search(&node)
                    .expect_err("Tvix bug: dir entry with name already exists");
                self.directories.insert(pos, node);
            }
            Node::Symlink(node) => {
                let pos = self
                    .symlinks
                    .binary_search(&node)
                    .expect_err("Tvix bug: dir entry with name already exists");
                self.symlinks.insert(pos, node);
            }
        }
    }
}

/// Struct to hold the state of an iterator over all nodes of a Directory.
///
/// Internally, this keeps peekable Iterators over all three lists of a
/// Directory message.
pub struct DirectoryNodesIterator<'a> {
    // directory: &Directory,
    i_directories: Peekable<std::slice::Iter<'a, DirectoryNode>>,
    i_files: Peekable<std::slice::Iter<'a, FileNode>>,
    i_symlinks: Peekable<std::slice::Iter<'a, SymlinkNode>>,
}

/// looks at two elements implementing NamedNode, and returns true if "left
/// is smaller / comes first".
///
/// Some(_) is preferred over None.
fn left_name_lt_right<A: NamedNode, B: NamedNode>(left: Option<&A>, right: Option<&B>) -> bool {
    match left {
        // if left is None, right always wins
        None => false,
        Some(left_inner) => {
            // left is Some.
            match right {
                // left is Some, right is None - left wins.
                None => true,
                Some(right_inner) => {
                    // both are Some - compare the name.
                    return left_inner.get_name() < right_inner.get_name();
                }
            }
        }
    }
}

impl Iterator for DirectoryNodesIterator<'_> {
    type Item = Node;

    // next returns the next node in the Directory.
    // we peek at all three internal iterators, and pick the one with the
    // smallest name, to ensure lexicographical ordering.
    // The individual lists are already known to be sorted.
    fn next(&mut self) -> Option<Self::Item> {
        if left_name_lt_right(self.i_directories.peek(), self.i_files.peek()) {
            // i_directories is still in the game, compare with symlinks
            if left_name_lt_right(self.i_directories.peek(), self.i_symlinks.peek()) {
                self.i_directories
                    .next()
                    .cloned()
                    .map(Node::Directory)
            } else {
                self.i_symlinks.next().cloned().map(Node::Symlink)
            }
        } else {
            // i_files is still in the game, compare with symlinks
            if left_name_lt_right(self.i_files.peek(), self.i_symlinks.peek()) {
                self.i_files.next().cloned().map(Node::File)
            } else {
                self.i_symlinks.next().cloned().map(Node::Symlink)
            }
        }
    }
}
