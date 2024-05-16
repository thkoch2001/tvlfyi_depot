use std::collections::HashMap;

use bstr::ByteSlice;
use prost::Message;

use petgraph::{
    graph::{DiGraph, NodeIndex},
    visit::{Bfs, DfsPostOrder, EdgeRef, Walker},
    Direction,
};
use tracing::instrument;

use super::order_validator::{LeavesToRootValidator, OrderValidator};
use crate::{
    proto::{self, Directory, DirectoryNode},
    B3Digest,
};

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("{0}")]
    ValidationError(String),
    #[error("{0}")]
    OrderError(#[from] super::order_validator::Error),
}

/// This can be used to validate a Directory closure (DAG of connected
/// Directories), and their insertion order.
///
/// Directories need to be inserted (via `add`), in an order from the leaves to
/// the root (DFS Post-Order).
/// During insertion, We validate as much as we can at that time:
///
///  - individual validation of Directory messages
///  - validation of insertion order (no upload of not-yet-known Directories)
///  - validation of size fields of referred Directories
///
/// Internally it keeps all received Directories in a directed graph,
/// with node weights being the Directories and edges pointing to child
/// directories.
///
/// Once all Directories have been inserted, a finalize function can be
/// called to get a (deduplicated and) validated list of directories, in
/// insertion order.
/// During finalize, a check for graph connectivity is performed too, to ensure
/// there's no disconnected components, and only one root.
#[derive(Default)]
pub struct ClosureValidator<O = LeavesToRootValidator> {
    // A directed graph, using Directory as node weight.
    // Edges point from parents to children.
    //
    // Nodes with None weigths might exist when a digest has been referred to but the directory
    // with this digest has not yet been sent.
    //
    // The option in the edge weight tracks the pending validation state of the respective edge, for example if
    // the child has not been added yet.
    graph: DiGraph<Option<Directory>, Option<DirectoryNode>>,

    // A lookup table from directory digest to node index.
    digest_to_node_ix: HashMap<B3Digest, NodeIndex>,

    order_validator: O,
}
fn check_edge(dir: &DirectoryNode, child: &Directory) -> Result<(), Error> {
    // Ensure the size specified in the child node matches our records.
    if dir.size != child.size() {
        return Err(Error::ValidationError(format!(
            "'{}' has wrong size, specified {}, recorded {}",
            dir.name.as_bstr(),
            dir.size,
            child.size(),
        )));
    }
    Ok(())
}

impl<O: OrderValidator> ClosureValidator<O> {
    pub(crate) fn with_order(order_validator: O) -> Self {
        Self {
            graph: Default::default(),
            digest_to_node_ix: Default::default(),
            order_validator,
        }
    }

    /// Parse a new Directory from its protobuf format and add it into the closure
    #[instrument(level = "trace", skip_all, err)]
    pub fn add_bytes(&mut self, bytes: bytes::Bytes) -> Result<(), Error> {
        let mut hasher = blake3::Hasher::new();
        let digest: B3Digest = hasher.update(&bytes).finalize().as_bytes().into();

        // Only decode the bytes if the order validator allows it
        if !self.order_validator.digest_allowed(&digest) {
            return Err(Error::ValidationError(format!(
                "unexpected digest: {:?}",
                &digest
            )));
        }

        let directory = proto::Directory::decode(bytes)
            .map_err(|e| Error::ValidationError(format!("failed to decode Directory: {}", e)))?;

        self.add_order_unchecked(directory)
    }

    /// Insert a new Directory into the closure
    /// If possible, use add_bytes to leave the parsing up to the ClosureValidator
    #[instrument(level = "trace", skip_all, fields(directory.digest=%directory.digest(), directory.size=%directory.size()), err)]
    pub fn add(&mut self, directory: proto::Directory) -> Result<(), Error> {
        let digest = directory.digest();
        if !self.order_validator.digest_allowed(&digest) {
            return Err(Error::ValidationError(format!(
                "unexpected digest: {:?}",
                &digest
            )));
        }
        self.add_order_unchecked(directory)
    }

    /// Adds a directory which has already been confirmed to be in-order to the graph.
    fn add_order_unchecked(&mut self, directory: proto::Directory) -> Result<(), Error> {
        // Do some basic validation
        directory
            .validate()
            .map_err(|e| Error::ValidationError(e.to_string()))?;

        let digest = directory.digest();

        for subdir in &directory.directories {
            let subdir_digest: B3Digest = subdir
                .digest
                .clone()
                .try_into()
                .map_err(|e| Error::ValidationError(format!("invalid digest: {}", e)))?;
            if subdir_digest == digest {
                return Err(Error::ValidationError(
                    "self-referential directories are not allowed".into(),
                ));
            }
        }

        // Tell the order validator about the new node
        self.order_validator.add_directory(&directory)?;

        // Teach the graph about the existence of a node with this digest
        let ix = *self
            .digest_to_node_ix
            .entry(digest)
            .or_insert_with(|| self.graph.add_node(None));

        if self.graph[ix].is_some() {
            // The node is already in the graph, there is nothing to do here.
            return Ok(());
        }

        // set up edges to all child directories
        for subdir in &directory.directories {
            let subdir_digest: B3Digest = subdir.digest.clone().try_into().unwrap();

            let child_ix = *self
                .digest_to_node_ix
                .entry(subdir_digest)
                .or_insert_with(|| self.graph.add_node(None));

            let pending_edge_check = match &self.graph[child_ix] {
                Some(child) => {
                    // child is already available, validate the edge now
                    check_edge(subdir, child)?;
                    None
                }
                None => Some(subdir.clone()), // pending validation
            };
            self.graph.add_edge(ix, child_ix, pending_edge_check);
        }

        // validate the edges from parents to this node
        // this is complicated because there is no edges_directed_mut :'c
        for edge_id in self
            .graph
            .edges_directed(ix, Direction::Incoming)
            .map(|edge_ref| edge_ref.id())
            .collect::<Vec<_>>()
            .into_iter()
        {
            let edge_weight = self
                .graph
                .edge_weight_mut(edge_id)
                .expect("edge not found")
                .take()
                .expect("edge is already validated");
            check_edge(&edge_weight, &directory)?;
        }

        // finally, store the directory information in the node weight
        self.graph[ix] = Some(directory);

        Ok(())
    }

    #[instrument(level = "trace", skip_all, err)]
    pub(crate) fn validate(&self) -> Result<(), Error> {
        // If there disconnected nodes, the order validator will complain
        let _root = match self.order_validator.root()? {
            Some(v) => v,
            None => return Ok(()), // Graph is empty, nothing to validate
        };

        // test that the graph is complete
        if self.graph.raw_nodes().iter().any(|n| n.weight.is_none()) {
            return Err(Error::ValidationError("graph is incomplete".into()));
        }

        Ok(())
    }

    /// Return the list of directories in from-root-to-leaves order.
    /// In case no elements have been inserted, returns an empty list.
    ///
    /// panics if the specified root is not in the graph
    #[instrument(level = "trace", skip_all)]
    pub(crate) fn drain_root_to_leaves(self) -> impl Iterator<Item = Directory> {
        let root = match self.order_validator.root().unwrap() {
            None => return itertools::Either::Left(std::iter::empty()),
            Some(v) => v,
        };

        // do a BFS traversal of the graph, starting with the root node
        let traversal = Bfs::new(&self.graph, self.digest_to_node_ix[&root]);

        let order = traversal.iter(&self.graph).collect::<Vec<_>>();

        let (mut nodes, _edges) = self.graph.into_nodes_edges();

        itertools::Either::Right(
            order
                .into_iter()
                .filter_map(move |i| nodes[i.index()].weight.take()),
        )
    }

    /// Return the list of directories in from-leaves-to-root order.
    /// In case no elements have been inserted, returns an empty list.
    ///
    /// panics when the specified root is not in the graph
    #[instrument(level = "trace", skip_all)]
    pub(crate) fn drain_leaves_to_root(self) -> impl Iterator<Item = Directory> {
        let root = match self.order_validator.root().unwrap() {
            None => return itertools::Either::Left(std::iter::empty()),
            Some(v) => v,
        };

        // do a DFS Post-Order traversal of the graph, starting with the root node
        let traversal = DfsPostOrder::new(&self.graph, self.digest_to_node_ix[&root]);

        let order = traversal.iter(&self.graph).collect::<Vec<_>>();

        let (mut nodes, _edges) = self.graph.into_nodes_edges();

        itertools::Either::Right(
            order
                .into_iter()
                .filter_map(move |i| nodes[i.index()].weight.take()),
        )
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        fixtures::{DIRECTORY_A, DIRECTORY_B, DIRECTORY_C},
        proto::{self, Directory},
    };
    use lazy_static::lazy_static;
    use rstest::rstest;

    lazy_static! {
        pub static ref BROKEN_DIRECTORY : Directory = Directory {
            symlinks: vec![proto::SymlinkNode {
                name: "".into(), // invalid name!
                target: "doesntmatter".into(),
            }],
            ..Default::default()
        };

        pub static ref BROKEN_PARENT_DIRECTORY: Directory = Directory {
            directories: vec![proto::DirectoryNode {
                name: "foo".into(),
                digest: DIRECTORY_A.digest().into(),
                size: DIRECTORY_A.size() + 42, // wrong!
            }],
            ..Default::default()
        };
    }

    use super::{ClosureValidator, LeavesToRootValidator};

    #[rstest]
    /// Uploading an empty directory should succeed.
    #[case::empty_directory(&[&*DIRECTORY_A], false, Some(vec![&*DIRECTORY_A]))]
    /// Uploading A, then B (referring to A) should succeed.
    #[case::simple_closure(&[&*DIRECTORY_A, &*DIRECTORY_B], false, Some(vec![&*DIRECTORY_A, &*DIRECTORY_B]))]
    /// Uploading A, then A, then C (referring to A twice) should succeed.
    /// We pretend to be a dumb client not deduping directories.
    #[case::same_child(&[&*DIRECTORY_A, &*DIRECTORY_A, &*DIRECTORY_C], false, Some(vec![&*DIRECTORY_A, &*DIRECTORY_C]))]
    /// Uploading A, then C (referring to A twice) should succeed.
    #[case::same_child_dedup(&[&*DIRECTORY_A, &*DIRECTORY_C], false, Some(vec![&*DIRECTORY_A, &*DIRECTORY_C]))]
    /// Uploading A, then C (referring to A twice), then B (itself referring to A) should fail during close,
    /// as B itself would be left unconnected.
    #[case::unconnected_node(&[&*DIRECTORY_A, &*DIRECTORY_C, &*DIRECTORY_B], false, None)]
    /// Uploading B (referring to A) should fail immediately, because A was never uploaded.
    #[case::dangling_pointer(&[&*DIRECTORY_B], true, None)]
    /// Uploading a directory failing validation should fail immediately.
    #[case::failing_validation(&[&*BROKEN_DIRECTORY], true, None)]
    /// Uploading a directory which refers to another Directory with a wrong size should fail.
    #[case::wrong_size_in_parent(&[&*DIRECTORY_A, &*BROKEN_PARENT_DIRECTORY], true, None)]
    fn test_uploads(
        #[case] directories_to_upload: &[&Directory],
        #[case] exp_fail_upload_last: bool,
        #[case] exp_finalize: Option<Vec<&Directory>>, // Some(_) if finalize successful, None if not.
    ) {
        let mut dcv = ClosureValidator::<LeavesToRootValidator>::default();
        let len_directories_to_upload = directories_to_upload.len();

        for (i, d) in directories_to_upload.iter().enumerate() {
            let resp = dcv.add((*d).clone());
            if i == len_directories_to_upload - 1 && exp_fail_upload_last {
                assert!(resp.is_err(), "expect last put to fail");

                // We don't really care anymore what finalize() would return, as
                // the add() failed.
                return;
            } else {
                assert!(resp.is_ok(), "expect put to succeed");
            }
        }

        // everything was uploaded successfully. Test finalize().
        let resp = dcv
            .validate()
            .map(|_| dcv.drain_leaves_to_root().collect::<Vec<_>>());

        match exp_finalize {
            Some(directories) => {
                assert_eq!(
                    Vec::from_iter(directories.iter().map(|e| (*e).to_owned())),
                    resp.expect("drain should succeed")
                );
            }
            None => {
                resp.expect_err("drain should fail");
            }
        }
    }
}
