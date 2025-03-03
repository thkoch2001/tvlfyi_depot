//! Imports from an archive (tarballs)

use std::collections::HashMap;

use petgraph::graph::{DiGraph, NodeIndex};
use petgraph::visit::{DfsPostOrder, EdgeRef};
use petgraph::Direction;
use tokio::io::AsyncRead;
use tokio_stream::StreamExt;
use tokio_tar::Archive;
use tracing::{instrument, warn, Level};

use crate::blobservice::BlobService;
use crate::directoryservice::DirectoryService;
use crate::import::{ingest_entries, IngestionEntry, IngestionError};
use crate::Node;

use super::blobs::{self, ConcurrentBlobUploader};

type TarPathBuf = std::path::PathBuf;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("unable to construct stream of entries: {0}")]
    Entries(std::io::Error),

    #[error("unable to read next entry: {0}")]
    NextEntry(std::io::Error),

    #[error("unable to read path for entry: {0}")]
    PathRead(std::io::Error),

    #[error("unable to convert path {0} for entry: {1}")]
    PathConvert(TarPathBuf, std::io::Error),

    #[error("unable to read size field for {0}: {1}")]
    Size(TarPathBuf, std::io::Error),

    #[error("unable to read mode field for {0}: {1}")]
    Mode(TarPathBuf, std::io::Error),

    #[error("unable to read link name field for {0}: {1}")]
    LinkName(TarPathBuf, std::io::Error),

    #[error("unsupported tar entry {0} type: {1:?}")]
    EntryType(TarPathBuf, tokio_tar::EntryType),

    #[error("symlink missing target {0}")]
    MissingSymlinkTarget(TarPathBuf),

    #[error("unexpected number of top level directory entries")]
    UnexpectedNumberOfTopLevelEntries,

    #[error(transparent)]
    BlobUploadError(#[from] blobs::Error),
}

/// Ingests elements from the given tar [`Archive`] into a the passed [`BlobService`] and
/// [`DirectoryService`].
#[instrument(skip_all, ret(level = Level::TRACE), err)]
pub async fn ingest_archive<BS, DS, R>(
    blob_service: BS,
    directory_service: DS,
    mut archive: Archive<R>,
) -> Result<Node, IngestionError<Error>>
where
    BS: BlobService + Clone + 'static,
    DS: DirectoryService,
    R: AsyncRead + Unpin,
{
    // Since tarballs can have entries in any arbitrary order, we need to
    // buffer all of the directory metadata so we can reorder directory
    // contents and entries to meet the requires of the castore.

    // In the first phase, collect up all the regular files and symlinks.
    let mut nodes = IngestionEntryGraph::new();

    let mut blob_uploader = ConcurrentBlobUploader::new(blob_service);

    let mut entries_iter = archive.entries().map_err(Error::Entries)?;
    while let Some(mut entry) = entries_iter.try_next().await.map_err(Error::NextEntry)? {
        let tar_path: TarPathBuf = entry.path().map_err(Error::PathRead)?.into();

        // construct a castore PathBuf, which we use in the produced IngestionEntry.
        let path = crate::path::PathBuf::from_host_path(tar_path.as_path(), true)
            .map_err(|e| Error::PathConvert(tar_path.clone(), e))?;

        let header = entry.header();
        let entry = match header.entry_type() {
            tokio_tar::EntryType::Regular
            | tokio_tar::EntryType::GNUSparse
            | tokio_tar::EntryType::Continuous => {
                let size = header
                    .size()
                    .map_err(|e| Error::Size(tar_path.clone(), e))?;

                let digest = blob_uploader
                    .upload(&path, size, &mut entry)
                    .await
                    .map_err(Error::BlobUploadError)?;

                let executable = entry
                    .header()
                    .mode()
                    .map_err(|e| Error::Mode(tar_path, e))?
                    & 64
                    != 0;

                IngestionEntry::Regular {
                    path,
                    size,
                    executable,
                    digest,
                }
            }
            tokio_tar::EntryType::Symlink => IngestionEntry::Symlink {
                target: entry
                    .link_name()
                    .map_err(|e| Error::LinkName(tar_path.clone(), e))?
                    .ok_or_else(|| Error::MissingSymlinkTarget(tar_path.clone()))?
                    .into_owned()
                    .into_os_string()
                    .into_encoded_bytes(),
                path,
            },
            // Push a bogus directory marker so we can make sure this directoy gets
            // created. We don't know the digest and size until after reading the full
            // tarball.
            tokio_tar::EntryType::Directory => IngestionEntry::Dir { path },

            tokio_tar::EntryType::XGlobalHeader | tokio_tar::EntryType::XHeader => continue,

            entry_type => return Err(Error::EntryType(tar_path, entry_type).into()),
        };

        nodes.add(entry)?;
    }

    blob_uploader.join().await.map_err(Error::BlobUploadError)?;

    let root_node = ingest_entries(
        directory_service,
        futures::stream::iter(nodes.finalize()?.into_iter().map(Ok)),
    )
    .await?;

    Ok(root_node)
}

/// Keep track of the directory structure of a file tree being ingested. This is used
/// for ingestion sources which do not provide any ordering or uniqueness guarantees
/// like tarballs.
///
/// If we ingest multiple entries with the same paths and both entries are not directories,
/// the newer entry will replace the latter entry, disconnecting the old node's children
/// from the graph.
///
/// Once all nodes are ingested a call to [IngestionEntryGraph::finalize] will return
/// a list of entries compute by performaing a DFS post order traversal of the graph
/// from the top-level directory entry.
///
/// This expects the directory structure to contain a single top-level directory entry.
/// An error is returned if this is not the case and ingestion will fail.
struct IngestionEntryGraph {
    graph: DiGraph<IngestionEntry, ()>,
    path_to_index: HashMap<crate::path::PathBuf, NodeIndex>,
    root_node: Option<NodeIndex>,
}

impl Default for IngestionEntryGraph {
    fn default() -> Self {
        Self::new()
    }
}

impl IngestionEntryGraph {
    /// Creates a new ingestion entry graph.
    pub fn new() -> Self {
        IngestionEntryGraph {
            graph: DiGraph::new(),
            path_to_index: HashMap::new(),
            root_node: None,
        }
    }

    /// Adds a new entry to the graph. Parent directories are automatically inserted.
    /// If a node exists in the graph with the same name as the new entry and both the old
    /// and new nodes are not directories, the node is replaced and is disconnected from its
    /// children.
    pub fn add(&mut self, entry: IngestionEntry) -> Result<NodeIndex, Error> {
        let path = entry.path().to_owned();

        let index = match self.path_to_index.get(entry.path()) {
            Some(&index) => {
                // If either the old entry or new entry are not directories, we'll replace the old
                // entry.
                if !entry.is_dir() || !self.get_node(index).is_dir() {
                    self.replace_node(index, entry);
                }

                index
            }
            None => self.graph.add_node(entry),
        };

        // for archives, a path with 1 component is the root node
        if path.components().count() == 1 {
            // We expect archives to contain a single root node, if there is another root node
            // entry with a different path name, this is unsupported.
            if let Some(root_node) = self.root_node {
                if self.get_node(root_node).path() != path.as_ref() {
                    return Err(Error::UnexpectedNumberOfTopLevelEntries);
                }
            }

            self.root_node = Some(index)
        } else if let Some(parent_path) = path.parent() {
            // Recursively add the parent node until it hits the root node.
            let parent_index = self.add(IngestionEntry::Dir {
                path: parent_path.to_owned(),
            })?;

            // Insert an edge from the parent directory to the child entry.
            self.graph.add_edge(parent_index, index, ());
        }

        self.path_to_index.insert(path, index);

        Ok(index)
    }

    /// Traverses the graph in DFS post order and collects the entries into a [Vec<IngestionEntry>].
    ///
    /// Unreachable parts of the graph are not included in the result.
    pub fn finalize(self) -> Result<Vec<IngestionEntry>, Error> {
        // There must be a root node.
        let Some(root_node_index) = self.root_node else {
            return Err(Error::UnexpectedNumberOfTopLevelEntries);
        };

        // The root node must be a directory.
        if !self.get_node(root_node_index).is_dir() {
            return Err(Error::UnexpectedNumberOfTopLevelEntries);
        }

        let mut traversal = DfsPostOrder::new(&self.graph, root_node_index);
        let mut nodes = Vec::with_capacity(self.graph.node_count());
        while let Some(node_index) = traversal.next(&self.graph) {
            nodes.push(self.get_node(node_index).clone());
        }

        Ok(nodes)
    }

    /// Replaces the node with the specified entry. The node's children are disconnected.
    ///
    /// This should never be called if both the old and new nodes are directories.
    fn replace_node(&mut self, index: NodeIndex, new_entry: IngestionEntry) {
        let entry = self
            .graph
            .node_weight_mut(index)
            .expect("Tvix bug: missing node entry");

        debug_assert!(!(entry.is_dir() && new_entry.is_dir()));

        // Replace the node itself.
        warn!(
            "saw duplicate entry in archive at path {:?}. old: {:?} new: {:?}",
            entry.path(),
            &entry,
            &new_entry
        );
        *entry = new_entry;

        // Remove any outgoing edges to disconnect the old node's children.
        let edges = self
            .graph
            .edges_directed(index, Direction::Outgoing)
            .map(|edge| edge.id())
            .collect::<Vec<_>>();
        for edge in edges {
            self.graph.remove_edge(edge);
        }
    }

    fn get_node(&self, index: NodeIndex) -> &IngestionEntry {
        self.graph
            .node_weight(index)
            .expect("Tvix bug: missing node entry")
    }
}

#[cfg(test)]
mod test {
    use std::sync::LazyLock;

    use super::{Error, IngestionEntryGraph};
    use crate::import::IngestionEntry;
    use crate::B3Digest;

    use rstest::rstest;

    pub static EMPTY_DIGEST: LazyLock<B3Digest> =
        LazyLock::new(|| blake3::hash(&[]).as_bytes().into());
    pub static DIR_A: LazyLock<IngestionEntry> = LazyLock::new(|| IngestionEntry::Dir {
        path: "a".parse().unwrap(),
    });
    pub static DIR_B: LazyLock<IngestionEntry> = LazyLock::new(|| IngestionEntry::Dir {
        path: "b".parse().unwrap(),
    });
    pub static DIR_A_B: LazyLock<IngestionEntry> = LazyLock::new(|| IngestionEntry::Dir {
        path: "a/b".parse().unwrap(),
    });
    pub static FILE_A: LazyLock<IngestionEntry> = LazyLock::new(|| IngestionEntry::Regular {
        path: "a".parse().unwrap(),
        size: 0,
        executable: false,
        digest: EMPTY_DIGEST.clone(),
    });
    pub static FILE_A_B: LazyLock<IngestionEntry> = LazyLock::new(|| IngestionEntry::Regular {
        path: "a/b".parse().unwrap(),
        size: 0,
        executable: false,
        digest: EMPTY_DIGEST.clone(),
    });
    pub static FILE_A_B_C: LazyLock<IngestionEntry> = LazyLock::new(|| IngestionEntry::Regular {
        path: "a/b/c".parse().unwrap(),
        size: 0,
        executable: false,
        digest: EMPTY_DIGEST.clone(),
    });

    #[rstest]
    #[case::implicit_directories(&[&*FILE_A_B_C], &[&*FILE_A_B_C, &*DIR_A_B, &*DIR_A])]
    #[case::explicit_directories(&[&*DIR_A, &*DIR_A_B, &*FILE_A_B_C], &[&*FILE_A_B_C, &*DIR_A_B, &*DIR_A])]
    #[case::inaccesible_tree(&[&*DIR_A, &*DIR_A_B, &*FILE_A_B], &[&*FILE_A_B, &*DIR_A])]
    fn node_ingestion_success(
        #[case] in_entries: &[&IngestionEntry],
        #[case] exp_entries: &[&IngestionEntry],
    ) {
        let mut nodes = IngestionEntryGraph::new();

        for entry in in_entries {
            nodes.add((*entry).clone()).expect("failed to add entry");
        }

        let entries = nodes.finalize().expect("invalid entries");

        let exp_entries: Vec<IngestionEntry> =
            exp_entries.iter().map(|entry| (*entry).clone()).collect();

        assert_eq!(entries, exp_entries);
    }

    #[rstest]
    #[case::no_top_level_entries(&[], Error::UnexpectedNumberOfTopLevelEntries)]
    #[case::multiple_top_level_dirs(&[&*DIR_A, &*DIR_B], Error::UnexpectedNumberOfTopLevelEntries)]
    #[case::top_level_file_entry(&[&*FILE_A], Error::UnexpectedNumberOfTopLevelEntries)]
    fn node_ingestion_error(#[case] in_entries: &[&IngestionEntry], #[case] exp_error: Error) {
        let mut nodes = IngestionEntryGraph::new();

        let result = (|| {
            for entry in in_entries {
                nodes.add((*entry).clone())?;
            }
            nodes.finalize()
        })();

        let error = result.expect_err("expected error");
        assert_eq!(error.to_string(), exp_error.to_string());
    }
}
