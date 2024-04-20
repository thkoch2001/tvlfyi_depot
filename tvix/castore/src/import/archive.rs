use std::{collections::HashMap, path::PathBuf};

use petgraph::graph::{DiGraph, NodeIndex};
use petgraph::visit::{DfsPostOrder, EdgeRef};
use petgraph::Direction;
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
    let mut nodes = IngestionEntryGraph::new();

    let mut entries_iter = archive.entries().map_err(Error::Archive)?;
    while let Some(mut entry) = entries_iter.try_next().await.map_err(Error::Archive)? {
        let path: PathBuf = entry.path().map_err(Error::Archive)?.into();

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
                    digest,
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

        nodes.add(entry);
    }

    ingest_entries(
        directory_service,
        futures::stream::iter(nodes.finalize().into_iter().map(Ok)),
    )
    .await
}

/// Keep track of the directory structure of a file tree being ingested. This is used
/// for ingestion sources which do not provide any ordering or uniqueness guarantees
/// like tarballs.
struct IngestionEntryGraph {
    graph: DiGraph<IngestionEntry, ()>,
    path_to_index: HashMap<PathBuf, NodeIndex>,
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
    /// If a node exists in the graph with the same name as the new entry, the node is replaced
    /// unless both the new node and the existing nodes are both directories.
    pub fn add(&mut self, entry: IngestionEntry) -> NodeIndex {
        let path = entry.path().to_path_buf();

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

        // A path with 1 component is the root node
        if path.components().count() == 1 {
            self.root_node = Some(index)
        } else {
            if let Some(parent_path) = path.parent() {
                let parent_index = self.add(IngestionEntry::Dir {
                    path: parent_path.to_path_buf(),
                });

                self.graph.add_edge(parent_index, index, ());
            };
        }

        self.path_to_index.insert(path, index);

        index
    }

    /// Traverses the graph in DFS port order and collects the entries into a [Vec<IngestionEntry>].
    ///
    /// Unreachable parts of the graph are not included in the result.
    pub fn finalize(self) -> Vec<IngestionEntry> {
        // TODO: Not sure if this should be allowed or not?
        let Some(root_node_index) = self.root_node else {
            return Vec::new();
        };

        let mut traversal = DfsPostOrder::new(&self.graph, root_node_index);
        let mut nodes = Vec::with_capacity(self.graph.node_count());
        while let Some(node_index) = traversal.next(&self.graph) {
            nodes.push(self.get_node(node_index).clone());
        }

        nodes
    }

    /// Replaces the node with the specified entry. All outgoing edges are removed for the node.
    fn replace_node(&mut self, index: NodeIndex, new_entry: IngestionEntry) {
        let entry = self
            .graph
            .node_weight_mut(index)
            .expect("Tvix bug: missing node entry");

        // Replace the node itself.
        *entry = new_entry;

        // Remove any outgoing edges.
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
    use std::ops::Deref;

    use crate::{import::IngestionEntry, B3Digest};

    use super::IngestionEntryGraph;

    use lazy_static::lazy_static;
    use rstest::rstest;

    lazy_static! {
        pub static ref EMPTY_DIGEST: B3Digest = blake3::hash(&[]).as_bytes().into();
        pub static ref DIR_A: IngestionEntry = IngestionEntry::Dir { path: "a".into() };
        pub static ref DIR_A_B: IngestionEntry = IngestionEntry::Dir { path: "a/b".into() };
        pub static ref FILE_A: IngestionEntry = IngestionEntry::Regular {
            path: "a".into(),
            size: 0,
            executable: false,
            digest: EMPTY_DIGEST.clone(),
        };
        pub static ref FILE_A_B_C: IngestionEntry = IngestionEntry::Regular {
            path: "a/b/c".into(),
            size: 0,
            executable: false,
            digest: EMPTY_DIGEST.clone(),
        };
    }

    #[rstest]
    #[case::empty(&[], &[])]
    #[case::implicit_directories(&[FILE_A_B_C.deref()], &[FILE_A_B_C.deref(), DIR_A_B.deref(), DIR_A.deref()])]
    #[case::explicit_directories(&[DIR_A.deref(), DIR_A_B.deref(), FILE_A_B_C.deref()], &[FILE_A_B_C.deref(), DIR_A_B.deref(), DIR_A.deref()])]
    #[case::inaccesible_tree(&[DIR_A.deref(), DIR_A_B.deref(), FILE_A.deref()], &[FILE_A.deref()])]
    fn node_ingestion(
        #[case] in_entries: &[&IngestionEntry],
        #[case] exp_entries: &[&IngestionEntry],
    ) {
        let mut nodes = IngestionEntryGraph::new();

        for entry in in_entries {
            nodes.add((*entry).clone());
        }

        let nodes = nodes.finalize();

        let exp_entries: Vec<IngestionEntry> =
            exp_entries.iter().map(|entry| (*entry).clone()).collect();

        assert_eq!(nodes, exp_entries);
    }
}
