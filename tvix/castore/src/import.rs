use crate::blobservice::BlobService;
use crate::directoryservice::DirectoryPutter;
use crate::directoryservice::DirectoryService;
use crate::proto::node::Node;
use crate::proto::Directory;
use crate::proto::DirectoryNode;
use crate::proto::FileNode;
use crate::proto::SymlinkNode;
use crate::Error as CastoreError;
use async_recursion::async_recursion;
use async_stream::try_stream;
use bytes::Bytes;
use futures::stream::Peekable;
use futures::Stream;
use futures::StreamExt;
use std::os::unix::ffi::OsStrExt;
use std::pin::pin;
use std::{
    fmt::Debug,
    os::unix::prelude::PermissionsExt,
    path::{Path, PathBuf},
};
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
}

impl From<CastoreError> for Error {
    fn from(value: CastoreError) -> Self {
        match value {
            CastoreError::InvalidRequest(_) => panic!("tvix bug"),
            CastoreError::StorageError(_) => panic!("error"),
        }
    }
}

/// Traverses a given path and returns a stream of all elements, from the root
/// to the leaves.
///
/// It does not follow symlinks, neither at the root, nor further down, they
/// will be yielded as actual symlinks.
// #[instrument(skip_all, fields(path=?p), err)]
pub async fn walk_path<P, F, S>(
    mut accept_fn: F,
    p: P,
) -> impl Stream<Item = Result<walkdir::DirEntry, walkdir::Error>>
where
    P: AsRef<Path> + Debug,
    F: FnMut(&walkdir::DirEntry) -> bool,
{
    let mut it = WalkDir::new(p.as_ref())
        .follow_links(false)
        .follow_root_links(false)
        .contents_first(false)
        .sort_by_file_name()
        .into_iter();

    try_stream! {
        loop {
            match it.next() {
                None => break,
                Some(Err(e)) => Err(e)?,
                Some(Ok(entry)) => {
                    if !accept_fn(&entry) {
                        it.skip_current_dir();
                        continue;
                    } else {
                        yield entry;
                    }
                }
            }
        }
    }
}

#[instrument(skip_all, fields(path=?p), err)]
pub async fn ingest_blob<BS>(
    blob_service: BS,
    p: impl AsRef<Path> + Debug,
) -> std::io::Result<FileNode>
where
    BS: AsRef<dyn BlobService>,
{
    // open the file at the path
    let mut f = tokio::fs::File::open(&p).await?;
    let metadata = f.metadata().await?;

    let mut blob_writer = blob_service.as_ref().open_write().await;
    tokio::io::copy(&mut f, &mut blob_writer);

    let blob_digest = blob_writer.close().await?;

    Ok(FileNode {
        name: Bytes::copy_from_slice(
            p.as_ref()
                .file_name()
                .ok_or_else(|| {
                    std::io::Error::new(std::io::ErrorKind::InvalidInput, "no valid path given")
                })?
                .as_bytes(),
        ),
        digest: blob_digest.into(),
        size: metadata.len(),
        executable: metadata.permissions().mode() & 0o100 != 0,
    })
}

#[instrument(skip_all, fields(path=?p), err)]
pub async fn ingest_symlink(p: impl AsRef<Path> + Debug) -> std::io::Result<SymlinkNode> {
    let target = tokio::fs::read_link(&p).await?;

    Ok(SymlinkNode {
        name: Bytes::copy_from_slice(
            p.as_ref()
                .file_name()
                .ok_or_else(|| {
                    std::io::Error::new(std::io::ErrorKind::InvalidInput, "no valid path given")
                })?
                .as_bytes(),
        ),
        target: Bytes::copy_from_slice(target.as_os_str().as_encoded_bytes()),
    })
}

/// Ingests elements yielded by the given stream, interacting with a
/// [BlobService] and [DirectoryService].
/// It peeks at the stream, and as soon as it's outside the same level as
/// previously received elements, returns a directory with all nodes received
/// so far.
///
/// It's not interacting with a PathInfoService (from tvix-store), or anything
/// else giving it a "non-content-addressed name".
/// It's up to the caller to possibly register it somewhere (and potentially
/// rename it based on some naming scheme)
#[async_recursion(?Send)]
pub async fn ingest_directory<S, BS>(
    elems: Peekable<S>,
    blob_service: BS,
    parent_path: Option<&'async_recursion Path>,
    directory_putter: Box<dyn DirectoryPutter>,
) -> std::io::Result<(DirectoryNode, Peekable<S>)>
where
    BS: AsRef<dyn BlobService> + Clone,
    S: Stream<Item = Result<walkdir::DirEntry, walkdir::Error>> + Unpin,
{
    let mut directory = Directory::default();

    let mut elems = elems;

    loop {
        let elem = pin!(elems).peek().await;
        match elem {
            None => {}
            Some(e) => {
                let e = e.map_err(|e| std::io::Error::new(std::io::ErrorKind::InvalidData, e))?;
                // Ensure the next element in the stream still has the same parent
                // as the one we're responsible for.
                if e.path().parent() != parent_path {
                    break;
                }

                // If it's a directory, recurse
                if e.file_type().is_dir() {
                    let (directory_node, rest_elems) = ingest_directory(
                        elems,
                        blob_service.clone(),
                        Some(e.path()),
                        directory_putter.clone(),
                    )
                    .await?;
                    directory.directories.push(directory_node);

                    elems = rest_elems;
                } else if e.file_type().is_file() {
                    directory
                        .files
                        .push(ingest_blob(blob_service.clone(), e.path()).await?);
                } else if e.file_type().is_symlink() {
                    directory.symlinks.push(ingest_symlink(e.path()).await?);
                } else {
                    return Err(std::io::Error::new(
                        std::io::ErrorKind::InvalidData,
                        "unknown file type",
                    ));
                }

                // advance the iterator
                let _ = pin!(&mut elems).next().await;
            }
        }
    }

    // When we arive here, we're either at the end of the stream, or peeked at
    // something we're not responsible for (different parent), so send off the
    // Directory struct and return the node pointing to it.

    let name = match parent_path {
        None => Bytes::from_static(&[]),
        Some(parent_path) => match parent_path.file_name() {
            None => Bytes::from_static(&[]),
            Some(name) => Bytes::copy_from_slice(name.as_bytes()),
        },
    };

    Ok((
        DirectoryNode {
            name,
            digest: directory.digest().into(),
            size: directory.size(),
        },
        elems,
    ))
}

pub async fn ingest_path<'a, BS, DS, P>(
    blob_service: BS,
    directory_service: DS,
    path: P,
) -> std::io::Result<Node>
where
    BS: AsRef<dyn BlobService> + Clone,
    DS: AsRef<dyn DirectoryService>,
    P: AsRef<Path> + Debug,
{
    // run walk_path (currently temporarily allowing all items), and turn it
    // into a peekable stream.
    let items = Box::new(walk_path(|_| true, path.as_ref()).await).peekable();

    // get the first element, describing the root node.
    let elem = pin!(items).next().await;
    debug_assert!(elem.is_some());
    let elem = elem.unwrap()?;
    let ft = elem.file_type();

    if ft.is_dir() {
        let mut directory_putter = directory_service.as_ref().put_multiple_start();

        let (root_directory_node, _rest) = ingest_directory(
            items,
            blob_service,
            Some(elem.path()),
            directory_putter.clone(),
        )
        .await?;

        // TODO: we need to close the putter to ensure it's flushed!

        Ok(Node::Directory(root_directory_node))
    } else if ft.is_file() {
        Ok(Node::File(
            ingest_blob(blob_service.clone(), path.as_ref()).await?,
        ))
    } else if ft.is_symlink() {
        Ok(Node::Symlink(ingest_symlink(path.as_ref()).await?))
    } else {
        Err(std::io::Error::new(
            std::io::ErrorKind::InvalidData,
            "unknown file type",
        ))
    }
}
