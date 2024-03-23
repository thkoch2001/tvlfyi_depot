use crate::blobservice::BlobService;
use crate::directoryservice::DirectoryPutter;
use crate::directoryservice::DirectoryService;
use crate::proto::node::Node;
use crate::proto::Directory;
use crate::proto::DirectoryNode;
use crate::proto::FileNode;
use crate::proto::SymlinkNode;
use crate::proto::ValidateDirectoryError;
use crate::Error as CastoreError;
use async_stream::stream;
use futures::pin_mut;
use futures::{Stream, StreamExt};
use std::fs::FileType;
use tokio::io;
use tokio::io::AsyncRead;
use tokio_tar::Archive;
use tracing::Level;

#[cfg(target_family = "unix")]
use std::os::unix::ffi::OsStrExt;

use std::{
    collections::HashMap,
    fmt::Debug,
    os::unix::prelude::PermissionsExt,
    path::{Path, PathBuf},
};
use tracing::instrument;
use walkdir::DirEntry;
use walkdir::WalkDir;

#[cfg(debug_assertions)]
use std::collections::HashSet;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("failed to upload directory at {0}: {1}")]
    UploadDirectoryError(PathBuf, CastoreError),

    #[error("invalid encoding encountered for entry {0:?}")]
    InvalidEncoding(PathBuf),

    #[error("unable to stat {0}: {1}")]
    UnableToStat(PathBuf, std::io::Error),

    #[error("unable to open {0}: {1}")]
    UnableToOpen(PathBuf, std::io::Error),

    #[error("unable to read {0}: {1}")]
    UnableToRead(PathBuf, std::io::Error),

    #[error("error reading from archive: {0}")]
    Archive(std::io::Error),

    #[error("unsupported file {0} type: {1:?}")]
    UnsupportedFileType(PathBuf, FileType),

    #[error("invalid directory contents {0}: {1}")]
    InvalidDirectory(PathBuf, ValidateDirectoryError),

    #[error("unsupported tar entry {0} type: {1:?}")]
    UnsupportedTarEntry(PathBuf, tokio_tar::EntryType),
}

impl From<CastoreError> for Error {
    fn from(value: CastoreError) -> Self {
        match value {
            CastoreError::InvalidRequest(_) => panic!("tvix bug"),
            CastoreError::StorageError(_) => panic!("error"),
        }
    }
}

impl From<Error> for std::io::Error {
    fn from(value: Error) -> Self {
        std::io::Error::new(std::io::ErrorKind::Other, value)
    }
}

/// Walk the filesystem at a given path and returns a level-keyed list of directory entries.
///
/// This is how [`ingest_path`] assembles the set of entries to pass on [`ingest_entries`].
/// This low-level function can be used if additional filtering or processing is required on the
/// entries.
///
/// Level here is in the context of graph theory, e.g. 2-level nodes
/// are nodes that are at depth 2.
///
/// This function will walk the filesystem using `walkdir` and will consume
/// `O(#number of entries)` space.
#[instrument(fields(path), err)]
pub fn walk_path_for_ingestion<P>(path: P) -> Result<Vec<Vec<DirEntry>>, Error>
where
    P: AsRef<Path> + std::fmt::Debug,
{
    let mut entries_per_depths: Vec<Vec<DirEntry>> = vec![Vec::new()];
    for entry in WalkDir::new(path.as_ref())
        .follow_links(false)
        .follow_root_links(false)
        .contents_first(false)
        .sort_by_file_name()
        .into_iter()
    {
        // Entry could be a NotFound, if the root path specified does not exist.
        let entry = entry.map_err(|e| {
            Error::UnableToOpen(
                PathBuf::from(path.as_ref()),
                e.into_io_error().expect("walkdir err must be some"),
            )
        })?;

        if entry.depth() >= entries_per_depths.len() {
            debug_assert!(
                entry.depth() == entries_per_depths.len(),
                "Received unexpected entry with depth {} during descent, previously at {}",
                entry.depth(),
                entries_per_depths.len()
            );

            entries_per_depths.push(vec![entry]);
        } else {
            entries_per_depths[entry.depth()].push(entry);
        }
    }

    Ok(entries_per_depths)
}

/// Convert a leveled-key vector of filesystem entries into a stream of
/// [DirEntry] in a way that honors the Merkle invariant, i.e. from bottom to top.
pub fn leveled_entries_to_stream(
    entries_per_depths: Vec<Vec<DirEntry>>,
) -> impl Stream<Item = DirEntry> {
    stream! {
        for level in entries_per_depths.into_iter().rev() {
            for entry in level.into_iter() {
                yield entry;
            }
        }
    }
}

/// Ingests the contents at a given path into the tvix store, interacting with a [BlobService] and
/// [DirectoryService]. It returns the root node or an error.
///
/// It does not follow symlinks at the root, they will be ingested as actual symlinks.
#[instrument(skip(blob_service, directory_service), fields(path), err)]
pub async fn ingest_path<'a, BS, DS, P>(
    blob_service: BS,
    directory_service: DS,
    path: P,
) -> Result<Node, Error>
where
    P: AsRef<Path> + std::fmt::Debug,
    BS: AsRef<dyn BlobService>,
    DS: AsRef<dyn DirectoryService>,
{
    // produce the leveled-key vector of DirEntry.
    let entries_per_depths = walk_path_for_ingestion(path)?;
    let direntry_stream = leveled_entries_to_stream(entries_per_depths);
    pin_mut!(direntry_stream);

    ingest_entries(blob_service, directory_service, direntry_stream).await
}

/// The Merkle invariant checker is an internal structure to perform bookkeeping of all directory
/// entries we are ingesting and verifying we are ingesting them in the right order.
///
/// That is, whenever we process an entry `L`, we would like to verify if we didn't process earlier
/// an entry `P` such that `P` is an **ancestor** of `L`.
///
/// If such a thing happened, it means that we have processed something like:
///
///```no_trust
///        A
///       / \
///      B   C
///     / \   \
///    G  F    P <--------- processed before this one
///           / \                                  |
///          D  E                                  |
///              \                                 |
///               L  <-----------------------------+
/// ```
///
/// This is exactly what must never happen.
///
/// Note: this checker is local, it can only see what happens on our side, not on the remote side,
/// i.e. the different remote services.
#[derive(Default)]
#[cfg(debug_assertions)]
struct MerkleInvariantChecker {
    seen: HashSet<PathBuf>,
}

#[cfg(debug_assertions)]
impl MerkleInvariantChecker {
    /// See a directory entry and remember it.
    fn see(&mut self, node: &DirEntry) {
        self.seen.insert(node.path().to_owned());
    }

    /// Returns a potential ancestor already seen for that directory entry.
    fn find_ancestor<'a>(&self, node: &'a DirEntry) -> Option<&'a Path> {
        node.path().ancestors().find(|p| self.seen.contains(*p))
    }
}

/// Ingests elements from the given stream of [`DirEntry`] into a the passed [`BlobService`] and
/// [`DirectoryService`].
/// It does not follow symlinks at the root, they will be ingested as actual symlinks.
#[instrument(skip_all, ret(level = Level::TRACE), err)]
pub async fn ingest_entries<'a, BS, DS, S>(
    blob_service: BS,
    directory_service: DS,
    #[allow(unused_mut)] mut direntry_stream: S,
) -> Result<Node, Error>
where
    BS: AsRef<dyn BlobService>,
    DS: AsRef<dyn DirectoryService>,
    S: Stream<Item = DirEntry> + std::marker::Unpin,
{
    #[cfg(debug_assertions)]
    let mut invariant_checker: MerkleInvariantChecker = Default::default();

    #[cfg(debug_assertions)]
    let mut direntry_stream = direntry_stream.inspect(|e| {
        // If we find an ancestor before we see this entry, this means that the caller
        // broke the contract, refer to the documentation of the invariant checker to
        // understand the reasoning here.
        if let Some(ancestor) = invariant_checker.find_ancestor(e) {
            panic!(
                "Tvix bug: merkle invariant checker discovered that {} was processed before {}!",
                ancestor.display(),
                e.path().display()
            );
        }

        invariant_checker.see(e);
    });

    // For a given path, this holds the [Directory] structs as they are populated.
    let mut directories: HashMap<PathBuf, Directory> = HashMap::default();
    let mut maybe_directory_putter: Option<Box<dyn DirectoryPutter>> = None;

    // We need to process a directory's children before processing
    // the directory itself in order to have all the data needed
    // to compute the hash.

    let root_node = loop {
        let entry = match direntry_stream.next().await {
            Some(entry) => entry,
            None => {
                // The last entry of the stream must have depth 0, after which
                // we break the loop manually.
                panic!("Tvix bug: unexpected end of stream");
            }
        };
        let file_type = entry.file_type();

        let node = if file_type.is_dir() {
            // If the entry is a directory, we traversed all its children (and
            // populated it in `directories`).
            // If we don't have it in there, it's an empty directory.
            let directory = directories
                .remove(entry.path())
                // In that case, it contained no children
                .unwrap_or_default();

            let directory_size = directory.size();
            let directory_digest = directory.digest();

            // Use the directory_putter to upload the directory.
            // If we don't have one yet (as that's the first one to upload),
            // initialize the putter.
            maybe_directory_putter
                .get_or_insert_with(|| directory_service.as_ref().put_multiple_start())
                .put(directory)
                .await?;

            Node::Directory(DirectoryNode {
                name: entry.file_name().as_bytes().to_owned().into(),
                digest: directory_digest.into(),
                size: directory_size,
            })
        } else if file_type.is_symlink() {
            let target: bytes::Bytes = std::fs::read_link(entry.path())
                .map_err(|e| Error::UnableToStat(entry.path().to_path_buf(), e))?
                .as_os_str()
                .as_bytes()
                .to_owned()
                .into();

            Node::Symlink(SymlinkNode {
                name: entry.file_name().as_bytes().to_owned().into(),
                target,
            })
        } else if file_type.is_file() {
            let metadata = entry
                .metadata()
                .map_err(|e| Error::UnableToStat(entry.path().to_path_buf(), e.into()))?;

            let mut file = tokio::fs::File::open(entry.path())
                .await
                .map_err(|e| Error::UnableToOpen(entry.path().to_path_buf(), e))?;

            let mut writer = blob_service.as_ref().open_write().await;

            if let Err(e) = tokio::io::copy(&mut file, &mut writer).await {
                return Err(Error::UnableToRead(entry.path().to_path_buf(), e));
            };

            let digest = writer
                .close()
                .await
                .map_err(|e| Error::UnableToRead(entry.path().to_path_buf(), e))?;

            Node::File(FileNode {
                name: entry.file_name().as_bytes().to_vec().into(),
                digest: digest.into(),
                size: metadata.len(),
                // If it's executable by the user, it'll become executable.
                // This matches nix's dump() function behaviour.
                executable: metadata.permissions().mode() & 64 != 0,
            })
        } else {
            return Err(Error::UnsupportedFileType(
                entry.path().to_path_buf(),
                file_type,
            ));
        };

        if entry.depth() == 0 {
            break node;
        } else {
            // calculate the parent path, and make sure we register the node there.
            // NOTE: entry.depth() > 0
            let parent_path = entry.path().parent().unwrap().to_path_buf();

            // record node in parent directory, creating a new [proto:Directory] if not there yet.
            let parent_directory = directories.entry(parent_path).or_default();
            match node {
                Node::Directory(e) => parent_directory.directories.push(e),
                Node::File(e) => parent_directory.files.push(e),
                Node::Symlink(e) => parent_directory.symlinks.push(e),
            }
        }
    };

    // if there were directories uploaded, make sure we flush the putter, so
    // they're all persisted to the backend.
    if let Some(mut directory_putter) = maybe_directory_putter {
        let root_directory_digest = directory_putter.close().await?;

        #[cfg(debug_assertions)]
        {
            if let Node::Directory(directory_node) = &root_node {
                debug_assert_eq!(
                    root_directory_digest,
                    directory_node
                        .digest
                        .to_vec()
                        .try_into()
                        .expect("invalid digest len")
                )
            } else {
                unreachable!("Tvix bug: directory putter initialized but no root directory node");
            }
        }
    };

    Ok(root_node)
}

/// Ingests elements from the given tar [`Archive`] into a the passed [`BlobService`] and
/// [`DirectoryService`].
#[instrument(skip_all, ret(level = Level::TRACE), err)]
pub async fn ingest_archive<'a, BS, DS, R>(
    blob_service: BS,
    directory_service: DS,
    mut archive: Archive<R>,
) -> Result<Node, Error>
where
    BS: AsRef<dyn BlobService> + Clone,
    DS: AsRef<dyn DirectoryService>,
    R: AsyncRead + Unpin,
{
    // Since tarballs can have entries in any arbitrary order, we need to
    // buffer all of the directory metadata so we can reorder directory
    // contents and entries to meet the requires of the castore.

    // In the first phase, collect up all the regular files and symlinks.
    let mut paths = HashMap::new();
    let mut entries = archive.entries().map_err(Error::Archive)?;
    while let Some(mut entry) = entries.try_next().await.map_err(Error::Archive)? {
        let path = entry.path().map_err(Error::Archive)?.into_owned();
        let name = path
            .file_name()
            .ok_or_else(|| {
                Error::Archive(io::Error::new(
                    io::ErrorKind::InvalidInput,
                    "invalid filename in archive",
                ))
            })?
            .as_bytes()
            .to_vec()
            .into();

        let node = match entry.header().entry_type() {
            tokio_tar::EntryType::Regular
            | tokio_tar::EntryType::GNUSparse
            | tokio_tar::EntryType::Continuous => {
                // FUTUREWORK: If the same path is overwritten in the tarball, we may leave
                // an unreferenced blob after uploading.
                let mut writer = blob_service.as_ref().open_write().await;
                let size = io::copy(&mut entry, &mut writer)
                    .await
                    .map_err(Error::Archive)?;
                let digest = writer.close().await.map_err(Error::Archive)?;
                Node::File(FileNode {
                    name,
                    digest: digest.into(),
                    size,
                    executable: entry.header().mode().map_err(Error::Archive)? & 64 != 0,
                })
            }
            tokio_tar::EntryType::Symlink => Node::Symlink(SymlinkNode {
                name,
                target: entry
                    .link_name()
                    .map_err(Error::Archive)?
                    .expect("symlink missing target")
                    .as_os_str()
                    .as_bytes()
                    .to_vec()
                    .into(),
            }),
            // Push a bogus directory marker so we can make sure this directoy gets
            // created. We don't know the digest and size until after reading the full
            // tarball.
            tokio_tar::EntryType::Directory => Node::Directory(DirectoryNode {
                name,
                digest: Default::default(),
                size: 0,
            }),

            tokio_tar::EntryType::XGlobalHeader | tokio_tar::EntryType::XHeader => continue,

            entry_type => return Err(Error::UnsupportedTarEntry(path, entry_type)),
        };

        paths.insert(path, node);
    }

    // In the second phase, construct all of the directories.

    // Collect into a list and then sort so all entries in the same directory
    // are next to each other.
    // We can detect boundaries between each directories to determine
    // when to construct or push directory entries.
    let mut ordered_paths = paths.into_iter().collect::<Vec<_>>();
    ordered_paths.sort_by(|a, b| a.0.cmp(&b.0));

    let mut directory_putter = directory_service.as_ref().put_multiple_start();

    // Start with an initial directory at the root.
    let mut dir_stack = vec![(PathBuf::from(""), Directory::default())];

    async fn pop_directory(
        dir_stack: &mut Vec<(PathBuf, Directory)>,
        directory_putter: &mut Box<dyn DirectoryPutter>,
    ) -> Result<DirectoryNode, Error> {
        let (path, directory) = dir_stack.pop().unwrap();

        directory
            .validate()
            .map_err(|e| Error::InvalidDirectory(path.to_path_buf(), e))?;

        let dir_node = DirectoryNode {
            name: path
                .file_name()
                .unwrap_or_default()
                .as_bytes()
                .to_vec()
                .into(),
            digest: directory.digest().into(),
            size: directory.size(),
        };

        if let Some((_, parent)) = dir_stack.last_mut() {
            parent.directories.push(dir_node.clone());
        }

        directory_putter.put(directory).await?;

        Ok(dir_node)
    }

    fn push_directories(path: &Path, dir_stack: &mut Vec<(PathBuf, Directory)>) {
        if path == dir_stack.last().unwrap().0 {
            return;
        }
        if let Some(parent) = path.parent() {
            push_directories(parent, dir_stack);
        }
        dir_stack.push((path.to_path_buf(), Directory::default()));
    }

    for (path, node) in ordered_paths.into_iter() {
        // Pop stack until the top dir is an ancestor of this entry.
        loop {
            let top = dir_stack.last().unwrap();
            if path.ancestors().any(|ancestor| ancestor == top.0) {
                break;
            }

            pop_directory(&mut dir_stack, &mut directory_putter).await?;
        }

        // For directories, just ensure the directory node exists.
        if let Node::Directory(_) = node {
            push_directories(&path, &mut dir_stack);
            continue;
        }

        // Push all ancestor directories onto the stack.
        push_directories(path.parent().unwrap(), &mut dir_stack);

        let top = dir_stack.last_mut().unwrap();
        debug_assert_eq!(Some(top.0.as_path()), path.parent());

        match node {
            Node::File(n) => top.1.files.push(n),
            Node::Symlink(n) => top.1.symlinks.push(n),
            // We already handled directories above.
            Node::Directory(_) => unreachable!(),
        }
    }

    let mut root_node = None;
    while !dir_stack.is_empty() {
        // If the root directory only has 1 directory entry, we return the child entry
        // instead... weeeee
        if dir_stack.len() == 1 && dir_stack.last().unwrap().1.directories.len() == 1 {
            break;
        }
        root_node = Some(pop_directory(&mut dir_stack, &mut directory_putter).await?);
    }
    let root_node = root_node.expect("no root node");

    let root_digest = directory_putter.close().await?;

    debug_assert_eq!(root_digest.as_slice(), &root_node.digest);

    Ok(Node::Directory(root_node))
}
