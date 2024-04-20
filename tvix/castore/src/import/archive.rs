use std::cmp::Ordering;
use std::path::Path;
use std::{collections::HashMap, path::PathBuf};

use tokio::io::AsyncRead;
use tokio_stream::StreamExt;
use tokio_tar::Archive;
use tracing::{instrument, Level};

use crate::blobservice::BlobService;
use crate::directoryservice::DirectoryService;
use crate::import::{ingest_entries, Error, IngestionEntry};
use crate::proto::node::Node;

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
    let mut entries = HashMap::new();

    // Adds the directory and all of its ancestors to the entries map.
    fn add_dir_all(entries: &mut HashMap<PathBuf, IngestionEntry>, path: &Path) {
        if entries
            .get(path)
            .map(|entry| entry.is_dir())
            .unwrap_or(false)
        {
            return;
        }
        if let Some(parent) = path.parent() {
            add_dir_all(entries, parent);
        }
        entries.insert(path.into(), IngestionEntry::Dir { path: path.into() });
    }

    let mut entries_iter = archive.entries().map_err(Error::Archive)?;
    while let Some(mut entry) = entries_iter.try_next().await.map_err(Error::Archive)? {
        let path: PathBuf = entry.path().map_err(Error::Archive)?.into();

        if let Some(parent) = path.parent() {
            add_dir_all(&mut entries, parent);
        }

        let entry = match entry.header().entry_type() {
            tokio_tar::EntryType::Regular
            | tokio_tar::EntryType::GNUSparse
            | tokio_tar::EntryType::Continuous => {
                // TODO: If the same path is overwritten in the tarball, we may leave
                // an unreferenced blob after uploading.
                let mut writer = blob_service.as_ref().open_write().await;
                let size = tokio::io::copy(&mut entry, &mut writer)
                    .await
                    .map_err(Error::Archive)?;
                let digest = writer.close().await.map_err(Error::Archive)?;

                IngestionEntry::Regular {
                    path: path.clone(),
                    size,
                    executable: entry.header().mode().map_err(Error::Archive)? & 64 != 0,
                    digest: Box::pin(async { Ok(digest) }),
                }
            }
            tokio_tar::EntryType::Symlink => IngestionEntry::Symlink {
                path: path.clone(),
                target: entry
                    .link_name()
                    .map_err(Error::Archive)?
                    .ok_or_else(|| Error::MissingSymlinkTarget(path.clone()))?
                    .into(),
            },
            // Push a bogus directory marker so we can make sure this directoy gets
            // created. We don't know the digest and size until after reading the full
            // tarball.
            tokio_tar::EntryType::Directory => IngestionEntry::Dir { path: path.clone() },

            tokio_tar::EntryType::XGlobalHeader | tokio_tar::EntryType::XHeader => continue,

            entry_type => return Err(Error::UnsupportedTarEntry(path, entry_type)),
        };

        entries.insert(path, entry);
    }

    // Sort the entries such that:
    // - Symlinks and regular files appear before all directories
    // - Directories are sorted by path length descending so that child directories
    //   appear before parent directories.
    let mut entries = entries.into_values().collect::<Vec<_>>();
    entries.sort_by(
        |a: &IngestionEntry, b: &IngestionEntry| match (a.is_dir(), b.is_dir()) {
            // Directories should be sorted by path length in descending order.
            (true, true) => b.path().as_os_str().len().cmp(&a.path().as_os_str().len()),
            // Non-directories should appear before directories.
            (true, false) => Ordering::Greater,
            (false, true) => Ordering::Less,
            // Ordering between non-directories does not matter.
            (false, false) => Ordering::Equal,
        },
    );

    ingest_entries(
        directory_service,
        futures::stream::iter(entries.into_iter().map(Ok)),
    )
    .await
}
