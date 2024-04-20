use std::path::Path;
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
    let mut nodes = ArchiveNodes::new();

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
        futures::stream::iter(nodes.drain().into_iter().map(Ok)),
    )
    .await
}

pub struct ArchiveNodes {
    graph: DiGraph<IngestionEntry, ()>,
    path_to_index: HashMap<PathBuf, NodeIndex>,
    root_node: Option<NodeIndex>,
}

impl Default for ArchiveNodes {
    fn default() -> Self {
        Self::new()
    }
}

impl ArchiveNodes {
    pub fn new() -> Self {
        ArchiveNodes {
            graph: DiGraph::new(),
            path_to_index: HashMap::new(),
            root_node: None,
        }
    }

    pub fn add(&mut self, entry: IngestionEntry) -> NodeIndex {
        let parent_index = match entry.path().parent() {
            Some(parent_path) if !parent_path.as_os_str().is_empty() => {
                self.create_dir_all(parent_path)
            }
            _ => None,
        };

        let path = entry.path().to_path_buf();

        let index = match self.path_to_index.get(entry.path()) {
            Some(index) => {
                // If we see a directory and the existing node is already a directory
                // no need to replace.
                if self.node_is_dir(*index) && entry.is_dir() {
                    *index
                } else {
                    self.replace_node(*index, entry)
                }
            }
            None => self.graph.add_node(entry),
        };

        if let Some(parent_index) = parent_index {
            self.graph.add_edge(parent_index, index, ());
        }

        if path.components().count() == 1 {
            self.root_node = Some(index)
        }

        self.path_to_index.insert(path, index);

        index
    }

    pub fn drain(self) -> Vec<IngestionEntry> {
        // TODO: Not sure if this should be allowed or not?
        let Some(root_node_index) = self.root_node else {
            return Vec::new();
        };

        let mut traversal = DfsPostOrder::new(&self.graph, root_node_index);
        let mut nodes = Vec::with_capacity(self.graph.node_count());
        while let Some(node_index) = traversal.next(&self.graph) {
            nodes.push(
                self.graph
                    .node_weight(node_index)
                    .expect("Tvix bug: missing node entry")
                    .clone(),
            );
        }

        nodes
    }

    fn create_dir_all(&mut self, path: &Path) -> Option<NodeIndex> {
        // Check if this directory already exists.
        if let Some(index) = self.path_to_index.get(path) {
            // If the node isn't a directory, we need to replace it.
            if self.node_is_dir(*index) {
                return Some(*index);
            }
        }

        Some(self.add(IngestionEntry::Dir {
            path: path.to_path_buf(),
        }))
    }

    fn node_is_dir(&self, index: NodeIndex) -> bool {
        let Some(entry) = self.graph.node_weight(index) else {
            panic!("Tvix bug: missing node entry");
        };

        entry.is_dir()
    }

    fn replace_node(&mut self, index: NodeIndex, new_entry: IngestionEntry) -> NodeIndex {
        let Some(entry) = self.graph.node_weight_mut(index) else {
            panic!("Tvix bug: missing node entry");
        };

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

        index
    }
}

#[cfg(test)]
mod test {
    use std::ops::Deref;

    use crate::{import::IngestionEntry, B3Digest};

    use super::ArchiveNodes;

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
        let mut nodes = ArchiveNodes::new();

        for entry in in_entries {
            nodes.add((*entry).clone());
        }

        let nodes = nodes.drain();

        let exp_entries: Vec<IngestionEntry> =
            exp_entries.iter().map(|entry| (*entry).clone()).collect();

        assert_eq!(nodes, exp_entries);
    }
}
