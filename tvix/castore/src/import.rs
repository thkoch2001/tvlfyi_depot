use crate::blobservice::BlobService;
use crate::directoryservice::DirectoryPutter;
use crate::directoryservice::DirectoryService;
use crate::proto::node::Node;
use crate::proto::Directory;
use crate::proto::DirectoryNode;
use crate::proto::FileNode;
use crate::proto::SymlinkNode;
use crate::Error as CastoreError;
use bytes::Bytes;
use futures::Stream;
use std::os::unix::ffi::OsStrExt;
use std::os::unix::ffi::OsStringExt;
use std::sync::Arc;
use std::{
    collections::HashMap,
    fmt::Debug,
    os::unix::prelude::PermissionsExt,
    path::{Path, PathBuf},
};
use tokio::io::AsyncRead;
use tokio_stream::StreamExt;
use tracing::instrument;
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

    #[error("error getting entry from stream: {0}")]
    FromStream(std::io::Error),

    #[error("ingestion stream was empty")]
    EmptyStream,
}

impl From<CastoreError> for Error {
    fn from(value: CastoreError) -> Self {
        match value {
            CastoreError::InvalidRequest(_) => panic!("tvix bug"),
            CastoreError::StorageError(_) => panic!("error"),
        }
    }
}

// This processes a given [walkdir::DirEntry] and returns a
// proto::node::Node, depending on the type of the entry.
//
// If the entry is a file, its contents are uploaded.
// If the entry is a directory, the Directory is uploaded as well.
// For this to work, it relies on the caller to provide the directory object
// with the previously returned (child) nodes.
//
// It assumes entries to be returned in "contents first" order, means this
// will only be called with a directory if all children of it have been
// visited. If the entry is indeed a directory, it'll also upload that
// directory to the store. For this, the so-far-assembled Directory object for
// this path needs to be passed in.
//
// It assumes the caller adds returned nodes to the directories it assembles.
#[instrument(skip_all)]
async fn process_entry<'a, R: AsyncRead + Unpin, P: AsRef<Path> + Debug>(
    blob_service: Arc<dyn BlobService>,
    directory_putter: &'a mut Box<dyn DirectoryPutter>,
    mut entry: IngestionEntry<R>,
    path: P,
    maybe_directory: Option<Directory>,
) -> Result<Node, Error> {
    let name = path
        .as_ref()
        .file_name()
        .expect("Entry path {path:?} should have a file name");
    match entry {
        IngestionEntry::Directory => {
            let directory = maybe_directory
                .expect("tvix bug: must be called with some directory in the case of directory");
            let directory_digest = directory.digest();
            let directory_size = directory.size();

            // upload this directory
            directory_putter
                .put(directory)
                .await
                .map_err(|e| Error::UploadDirectoryError(path.as_ref().to_path_buf(), e))?;

            return Ok(Node::Directory(DirectoryNode {
                name: name.as_bytes().to_owned().into(),
                digest: directory_digest.into(),
                size: directory_size,
            }));
        }
        IngestionEntry::Symlink { target } => {
            return Ok(Node::Symlink(SymlinkNode {
                name: name.as_bytes().to_owned().into(),
                target: Bytes::from(target),
            }));
        }
        IngestionEntry::File {
            ref mut reader,
            executable,
        } => {
            let mut writer = blob_service.open_write().await;

            let size = tokio::io::copy(reader, &mut writer)
                .await
                .map_err(|e| Error::UnableToRead(path.as_ref().to_path_buf(), e))?;

            let digest = writer.close().await?;

            return Ok(Node::File(FileNode {
                name: name.as_bytes().to_owned().into(),
                digest: digest.into(),
                size: size as u32,
                executable: executable,
            }));
        }
    }
}

pub enum IngestionEntry<R: AsyncRead> {
    Directory,
    Symlink { target: Vec<u8> },
    File { reader: R, executable: bool },
}

async fn walkdir_entry_to_ingestion_entry(
    maybe_entry: walkdir::Result<walkdir::DirEntry>,
) -> std::io::Result<(PathBuf, IngestionEntry<tokio::fs::File>)> {
    let entry = maybe_entry?;
    Ok((
        // TODO: should the ingestion root be included here?
        entry.path().to_path_buf(),
        if entry.file_type().is_dir() {
            IngestionEntry::Directory
        } else if entry.file_type().is_file() {
            IngestionEntry::File {
                // If it's executable by the user, it'll become executable.
                // This matches nix's dump() function behaviour.
                executable: entry.metadata()?.permissions().mode() & 0o100 != 0,
                reader: tokio::fs::File::open(entry.path()).await?,
            }
        } else if entry.file_type().is_symlink() {
            IngestionEntry::Symlink {
                target: tokio::fs::read_link(entry.path())
                    .await?
                    .into_os_string()
                    .into_vec(),
            }
        } else {
            todo!();
        },
    ))
}

/// Ingest contents from a filesystem into the blob and directory
/// service, returning a node for the given path or an error.
///
/// It does not follow symlinks at the root, they will be ingested as actual
/// symlinks.
pub async fn ingest_path_from_filesystem<P: AsRef<Path> + Debug>(
    blob_service: Arc<dyn BlobService>,
    directory_service: Arc<dyn DirectoryService>,
    p: P,
) -> Result<Node, Error> {
    let entries_iter = WalkDir::new(p)
        .follow_links(false)
        .follow_root_links(false)
        // We need to process a directory's children before processing
        // the directory itself in order to have all the data needed
        // to compute the hash.
        .contents_first(true)
        .sort_by_file_name();
    let ingestion_entries_stream =
        tokio_stream::iter(entries_iter).then(walkdir_entry_to_ingestion_entry);

    ingest_path(blob_service, directory_service, ingestion_entries_stream).await
}

/// Ingests contents into the tvix store, interacting with a
/// [BlobService] and [DirectoryService]. It returns the node
/// corresponding to the last entry in the stream, or an error.
///
/// contents should be a stream, containing pairs of paths and
/// IngestionEntry items. If the path being ingested is a directory,
/// this should include all contents of the directory, recursively,
/// and in a topological (deep-first) order. If it's a file or symlink,
/// it should only contain the one entry.
///
/// It is expected that everything in contents is connected in some
/// way to the last node in the stream. Ingesting disconnected graphs
/// of nodes will not currently cause an error, but no references to
/// the disconnected nodes will be returned, and the behaviour may
/// become stricter in the future.
///
/// It's not interacting with a PathInfoService (from tvix-store), or anything
/// else giving it a "non-content-addressed name".
/// It's up to the caller to possibly register it somewhere (and potentially
/// rename it based on some naming scheme).
#[instrument(skip_all)]
pub async fn ingest_path<P: AsRef<Path> + Debug, R: AsyncRead + Unpin>(
    blob_service: Arc<dyn BlobService>,
    directory_service: Arc<dyn DirectoryService>,
    contents: impl Stream<Item = std::io::Result<(P, IngestionEntry<R>)>>,
) -> Result<Node, Error> {
    let mut directories: HashMap<PathBuf, Directory> = HashMap::default();

    // TODO: validate that contents of a directory never come after the directory itself

    // TODO: pass this one instead?
    let mut directory_putter = directory_service.put_multiple_start();

    let mut last_node = None;

    tokio::pin!(contents);
    while let Some(maybe_entry) = contents.next().await {
        let (path, entry) = maybe_entry.map_err(Error::FromStream)?;

        // process_entry wants an Option<Directory> in case the entry points to a directory.
        // make sure to provide it.
        // If the directory has contents, we already have it in
        // `directories` due to the use of contents_first on WalkDir.
        let maybe_directory = if let IngestionEntry::Directory = entry {
            Some(
                directories
                    .entry(path.as_ref().to_path_buf())
                    .or_default()
                    .clone(),
            )
        } else {
            None
        };

        let node = process_entry(
            blob_service.clone(),
            &mut directory_putter,
            entry,
            &path,
            maybe_directory,
        )
        .await?;

        // calculate the parent path, and make sure we register the node there.
        // NOTE: entry.depth() > 0
        let parent_path = path.as_ref().parent().unwrap().to_path_buf();

        // record node in parent directory, creating a new [proto:Directory] if not there yet.
        let parent_directory = directories.entry(parent_path).or_default();
        match &node {
            Node::Directory(e) => parent_directory.directories.push(e.clone()),
            Node::File(e) => parent_directory.files.push(e.clone()),
            Node::Symlink(e) => parent_directory.symlinks.push(e.clone()),
        }
        last_node = Some(node);
    }
    last_node.ok_or(Error::EmptyStream)
}
