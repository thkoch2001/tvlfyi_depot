use std::io::{Cursor, Write};
use std::sync::Arc;
use std::{collections::HashMap, path::PathBuf};

use petgraph::graph::{DiGraph, NodeIndex};
use petgraph::visit::{DfsPostOrder, EdgeRef};
use petgraph::Direction;
use tokio::io::AsyncRead;
use tokio::sync::Semaphore;
use tokio::task::JoinSet;
use tokio_stream::StreamExt;
use tokio_tar::Archive;
use tokio_util::io::InspectReader;
use tracing::{instrument, Level};

use crate::blobservice::BlobService;
use crate::directoryservice::DirectoryService;
use crate::import::{ingest_entries, Error, IngestionEntry};
use crate::proto::node::Node;
use crate::B3Digest;

const ASYNC_UPLOAD_THRESHOLD: u64 = 1024 * 1024;
const ASYNC_UPLOAD_CONCURRENCY: usize = 128;

/// Ingests elements from the given tar [`Archive`] into a the passed [`BlobService`] and
/// [`DirectoryService`].
#[instrument(skip_all, ret(level = Level::TRACE), err)]
pub async fn ingest_archive<BS, DS, R>(
    blob_service: BS,
    directory_service: DS,
    mut archive: Archive<R>,
) -> Result<Node, Error>
where
    BS: BlobService + Clone + 'static,
    DS: AsRef<dyn DirectoryService>,
    R: AsyncRead + Unpin,
{
    // Since tarballs can have entries in any arbitrary order, we need to
    // buffer all of the directory metadata so we can reorder directory
    // contents and entries to meet the requires of the castore.

    // In the first phase, collect up all the regular files and symlinks.
    let mut nodes = IngestionEntryGraph::new();

    let semaphore = Arc::new(Semaphore::new(ASYNC_UPLOAD_CONCURRENCY));
    let mut async_blob_uploads: JoinSet<Result<(), Error>> = JoinSet::new();

    let mut entries_iter = archive.entries().map_err(Error::Archive)?;
    while let Some(mut entry) = entries_iter.try_next().await.map_err(Error::Archive)? {
        let path: PathBuf = entry.path().map_err(Error::Archive)?.into();

        let header = entry.header();
        let entry = match header.entry_type() {
            tokio_tar::EntryType::Regular
            | tokio_tar::EntryType::GNUSparse
            | tokio_tar::EntryType::Continuous => {
                let header_size = header.size().map_err(Error::Archive)?;

                // If the blob is small enough, read it off the wire, compute the digest,
                // and upload it to the [BlobService] in the background.
                let (size, digest) = if header_size <= ASYNC_UPLOAD_THRESHOLD {
                    let mut buffer = Vec::with_capacity(header_size as usize);
                    let mut hasher = blake3::Hasher::new();
                    let mut reader = InspectReader::new(&mut entry, |bytes| {
                        hasher.write(&bytes).unwrap();
                    });

                    let size = tokio::io::copy(&mut reader, &mut buffer)
                        .await
                        .map_err(Error::Archive)?;

                    let digest: B3Digest = hasher.finalize().as_bytes().into();

                    {
                        let blob_service = blob_service.clone();
                        let permit = semaphore.clone().acquire_owned().await.unwrap();
                        let digest = digest.clone();
                        async_blob_uploads.spawn({
                            async move {
                                let mut writer = blob_service.open_write().await;

                                tokio::io::copy(&mut Cursor::new(buffer), &mut writer)
                                    .await
                                    .map_err(Error::Archive)?;

                                let blob_digest = writer.close().await.map_err(Error::Archive)?;

                                assert_eq!(digest, blob_digest, "Tvix bug: blob digest mismatch");

                                drop(permit);
                                Ok(())
                            }
                        });
                    }

                    (size, digest)
                } else {
                    let mut writer = blob_service.open_write().await;

                    let size = tokio::io::copy(&mut entry, &mut writer)
                        .await
                        .map_err(Error::Archive)?;

                    let digest = writer.close().await.map_err(Error::Archive)?;

                    (size, digest)
                };

                IngestionEntry::Regular {
                    path,
                    size,
                    executable: entry.header().mode().map_err(Error::Archive)? & 64 != 0,
                    digest,
                }
            }
            tokio_tar::EntryType::Symlink => IngestionEntry::Symlink {
                target: entry
                    .link_name()
                    .map_err(Error::Archive)?
                    .ok_or_else(|| Error::MissingSymlinkTarget(path.clone()))?
                    .into(),
                path,
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

    while let Some(result) = async_blob_uploads.join_next().await {
        result.unwrap()?;
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
    /// If a node exists in the graph with the same name as the new entry and both the old
    /// and new nodes are not directories, the node is replaced and is disconnected from its
    /// children.
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
        } else if let Some(parent_path) = path.parent() {
            let parent_index = self.add(IngestionEntry::Dir {
                path: parent_path.to_path_buf(),
            });

            // Insert an edge from the parent directory to the child entry.
            self.graph.add_edge(parent_index, index, ());
        }

        self.path_to_index.insert(path, index);

        index
    }

    /// Traverses the graph in DFS post order and collects the entries into a [Vec<IngestionEntry>].
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
