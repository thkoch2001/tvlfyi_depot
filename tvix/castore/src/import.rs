use crate::blobservice::BlobService;
use crate::directoryservice::DirectoryPutter;
use crate::directoryservice::DirectoryService;
use crate::proto::node::Node;
use crate::proto::Directory;
use crate::proto::DirectoryNode;
use crate::proto::FileNode;
use crate::proto::SymlinkNode;
use crate::Error as CastoreError;
use std::ops::Deref;
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
}

impl From<CastoreError> for Error {
    fn from(value: CastoreError) -> Self {
        match value {
            CastoreError::InvalidRequest(_) => panic!("tvix bug"),
            CastoreError::StorageError(_) => panic!("error"),
        }
    }
}

/// This processes a given [walkdir::DirEntry] and returns a
/// proto::node::Node, depending on the type of the entry.
///
/// If the entry is a file, its contents are uploaded.
/// If the entry is a directory, the Directory is uploaded as well.
/// For this to work, it relies on the caller to provide the directory object
/// with the previously returned (child) nodes.
///
/// It assumes entries to be returned in "contents first" order, means this
/// will only be called with a directory if all children of it have been
/// visited. If the entry is indeed a directory, it'll also upload that
/// directory to the store. For this, the so-far-assembled Directory object for
/// this path needs to be passed in.
///
/// It assumes the caller adds returned nodes to the directories it assembles.
#[instrument(skip_all, fields(entry.file_type=?&entry.file_type(),entry.path=?entry.path()))]
async fn process_entry<'a, BS>(
    blob_service: BS,
    directory_putter: &'a mut Box<dyn DirectoryPutter>,
    entry: &'a walkdir::DirEntry,
    maybe_directory: Option<Directory>,
) -> Result<Node, Error>
where
    BS: Deref<Target = dyn BlobService> + Clone,
{
    let file_type = entry.file_type();

    if file_type.is_dir() {
        let directory = maybe_directory
            .expect("tvix bug: must be called with some directory in the case of directory");
        let directory_digest = directory.digest();
        let directory_size = directory.size();

        // upload this directory
        directory_putter
            .put(directory)
            .await
            .map_err(|e| Error::UploadDirectoryError(entry.path().to_path_buf(), e))?;

        return Ok(Node::Directory(DirectoryNode {
            name: entry.file_name().as_bytes().to_owned().into(),
            digest: directory_digest.into(),
            size: directory_size,
        }));
    }

    if file_type.is_symlink() {
        let target: bytes::Bytes = std::fs::read_link(entry.path())
            .map_err(|e| Error::UnableToStat(entry.path().to_path_buf(), e))?
            .as_os_str()
            .as_bytes()
            .to_owned()
            .into();

        return Ok(Node::Symlink(SymlinkNode {
            name: entry.file_name().as_bytes().to_owned().into(),
            target,
        }));
    }

    if file_type.is_file() {
        let metadata = entry
            .metadata()
            .map_err(|e| Error::UnableToStat(entry.path().to_path_buf(), e.into()))?;

        let mut file = tokio::fs::File::open(entry.path())
            .await
            .map_err(|e| Error::UnableToOpen(entry.path().to_path_buf(), e))?;

        let mut writer = blob_service.open_write().await;

        if let Err(e) = tokio::io::copy(&mut file, &mut writer).await {
            return Err(Error::UnableToRead(entry.path().to_path_buf(), e));
        };

        let digest = writer
            .close()
            .await
            .map_err(|e| Error::UnableToRead(entry.path().to_path_buf(), e))?;

        return Ok(Node::File(FileNode {
            name: entry.file_name().as_bytes().to_vec().into(),
            digest: digest.into(),
            size: metadata.len(),
            // If it's executable by the user, it'll become executable.
            // This matches nix's dump() function behaviour.
            executable: metadata.permissions().mode() & 64 != 0,
        }));
    }
    todo!("handle other types")
}

/// Ingests the contents at the given path into the tvix store,
/// interacting with a [BlobService] and [DirectoryService].
/// It returns the root node or an error.
///
/// It does not follow symlinks at the root, they will be ingested as actual
/// symlinks.
///
/// It's not interacting with a PathInfoService (from tvix-store), or anything
/// else giving it a "non-content-addressed name".
/// It's up to the caller to possibly register it somewhere (and potentially
/// rename it based on some naming scheme)
#[instrument(skip(blob_service, directory_service), fields(path=?p), err)]
pub async fn ingest_path<BS, DS, P>(
    blob_service: BS,
    directory_service: DS,
    p: P,
) -> Result<Node, Error>
where
    P: AsRef<Path> + Debug,
    BS: Deref<Target = dyn BlobService> + Clone,
    DS: Deref<Target = dyn DirectoryService>,
{
    let mut directories: HashMap<PathBuf, Directory> = HashMap::default();

    let mut directory_putter = directory_service.put_multiple_start();

    struct ContentsFirstWalkDirIterator {
        seen_on_the_path: Vec<DirEntry>,
        siblings: Vec<DirEntry>,
        peek_next: Option<DirEntry>,
        walkdir_iterator: <WalkDir as IntoIterator>::IntoIter,
    }

    impl ContentsFirstWalkDirIterator {
        pub fn new(walkdir: WalkDir) -> Self {
            Self {
                seen_on_the_path: Vec::new(),
                siblings: Vec::new(),
                peek_next: None,
                walkdir_iterator: walkdir.into_iter(),
            }
        }

        fn walk_until_leaves(&mut self, entry: DirEntry) -> walkdir::Result<DirEntry> {
            // Walking until the leaves means that you compute the increasing sequence of nodes by
            // next-ing the iterator until you find a node of depth smaller.
            let mut path = vec![entry];

            while let Some(peeked) = self.walkdir_iterator.next() {
                match peeked {
                    Err(err) => return Err(err),
                    Ok(peeked) => {
                        // We are increasing strictly, i.e. descending.
                        if path.last().unwrap().depth() < peeked.depth() {
                            // So, if peeked is a directory, it can be descended in further.
                            // Otherwise, it's a leaf.
                            // Because, we are descending in DFS according to `walkdir`, this leaf
                            // cannot be a intermediate leaf, it must be a leaf for which there's
                            // no sibling having a deeper subtree.
                            // So we can trust this is a sibling.
                            if !peeked.file_type().is_dir() {
                                self.siblings.push(peeked);
                            } else {
                                // We keep going inside the directory otherwise.
                                path.push(peeked);
                            }
                        } else {
                            // We didn't decrease strictly, either we are stagnating or we are
                            // decreasing.
                            // Therefore, we found our leaves.
                            // They are exactly contained inside `self.siblings` at the moment.
                            self.peek_next = Some(peeked);
                            break;
                        }
                    }
                }
            }

            self.seen_on_the_path.append(&mut path);
            // `seen_on_the_path` looks like `[ path so far ; entry ; new path encountered here until leaves ]` now.

            debug_assert!(
                self.siblings
                    .windows(2)
                    .all(|pairs| pairs[0].depth() == pairs[1].depth()),
                "Siblings should all exist on the same level."
            );
            debug_assert!(!self.siblings.is_empty(),
                "After a walk until leaves, leaves cannot be empty as they should at most contain the input argument, which is by definition, a leaf, in such cases.");
            debug_assert!(self.peek_next.is_none(),
                "The peeking pointer should be always consumed before we walk new leaves, otherwise the state machine is an incorrect state.");

            Ok(self.siblings.pop().unwrap())
        }
    }

    impl Iterator for ContentsFirstWalkDirIterator {
        type Item = walkdir::Result<DirEntry>;

        fn next(&mut self) -> Option<Self::Item> {
            if let Some(sibling) = self.siblings.pop() {
                return Some(Ok(sibling));
            }

            if self.siblings.is_empty() {
                if let Some(parent) = self.seen_on_the_path.pop() {
                    return Some(Ok(parent));
                }
            }

            // We would like to determine if we can pop further nodes on the path
            // and return them or not.
            if let Some(parent) = self.seen_on_the_path.last() {
                // We consume the insides of `peek_next`
                // to make it easier to pass it to `walk_until_leaves` which should consume it
                // further.
                if let Some(peeked) = std::mem::take(&mut self.peek_next) {
                    // Is the peeked path a suffix of parent?
                    // If so, this means that this parent has not been explored fully yet.
                    // We will walk until its leaves again.
                    if peeked.path().starts_with(parent.path()) {
                        // `walk_until_leaves` is responsible to populate `peek_next` again.
                        return Some(self.walk_until_leaves(peeked));
                    } else {
                        self.peek_next = Some(peeked);
                        // We can pop this parent again.
                        return Some(Ok(self.seen_on_the_path.pop().unwrap()));
                    }
                } else {
                    // If we arrived here, we consumed all the internal iterator, let's just flush
                    // everything we have.
                    return Some(Ok(self.seen_on_the_path.pop().unwrap()));
                }
            }

            // If have anything else in the peek buffer, return it now.
            if let Some(peeked) = std::mem::take(&mut self.peek_next) {
                return Some(Ok(peeked));
            }

            debug_assert!(
                self.seen_on_the_path.is_empty(),
                "Path seen so far is not empty!"
            );
            debug_assert!(self.siblings.is_empty(), "Siblings were not exhausted!");
            debug_assert!(self.peek_next.is_none(), "Peek buffer was not consumed!");

            // We exhausted all siblings, our current path (i.e. self.seen_on_the_path = []),
            // and our peek buffer.
            // Therefore, we are now moving on another root in the forest.
            match self.walkdir_iterator.next() {
                None => None,
                Some(Err(err)) => Some(Err(err)),
                Some(Ok(entry)) => Some(self.walk_until_leaves(entry)),
            }
        }
    }

    let internal_walker = WalkDir::new(p.as_ref())
        .follow_links(false)
        .follow_root_links(false)
        .contents_first(false)
        .sort_by_file_name();

    // We need to process a directory's children before processing
    // the directory itself in order to have all the data needed
    // to compute the hash.
    let walker = ContentsFirstWalkDirIterator::new(internal_walker);

    for entry in walker {
        // Entry could be a NotFound, if the root path specified does not exist.
        let entry = entry.map_err(|e| {
            Error::UnableToOpen(
                PathBuf::from(p.as_ref()),
                e.into_io_error().expect("walkdir err must be some"),
            )
        })?;

        // process_entry wants an Option<Directory> in case the entry points to a directory.
        // make sure to provide it.
        // If the directory has contents, we already have it in
        // `directories` due to the use of contents_first on WalkDir.
        let maybe_directory: Option<Directory> = {
            if entry.file_type().is_dir() {
                Some(
                    directories
                        .entry(entry.path().to_path_buf())
                        .or_default()
                        .clone(),
                )
            } else {
                None
            }
        };

        let node = process_entry(
            blob_service.clone(),
            &mut directory_putter,
            &entry,
            maybe_directory,
        )
        .await?;

        if entry.depth() == 0 {
            // Make sure all the directories are flushed.
            if entry.file_type().is_dir() {
                directory_putter.close().await?;
            }
            return Ok(node);
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
    }
    // unreachable, we already bailed out before if root doesn't exist.
    unreachable!()
}
