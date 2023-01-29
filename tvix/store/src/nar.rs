//! This provides some common "client-side" libraries to interact with a tvix-
//! store, in this case to render NAR.
use anyhow::Result;

use crate::proto::{self, NamedNode};

pub trait Blob: std::io::BufRead /* + std::io::Seek */ {}

pub trait StoreClient: Send + Sync {
    fn open_blob(&self, digest: Vec<u8>) -> std::io::Result<Box<dyn Blob>>;

    // TODO: stat_blob, put_blob?
    fn get_directory(&self, digest: Vec<u8>) -> std::io::Result<Option<proto::Directory>>;

    // TODO: put_directory
}

/// Consumes a [proto::node::Node] pointing to the root of a (store) path,
/// and writes the contents in NAR serialization to the passed
/// [std::io::Write].
///
/// It uses a [StoreClient] to do the necessary lookups as it traverses the
/// structure.
pub fn write_nar<W: std::io::Write, SC: StoreClient>(
    w: &mut W,
    proto_root_node: proto::node::Node,
    store_client: &mut SC,
) -> Result<()> {
    // Initialize tvix_nar
    let nar_root_node = tvix_nar::open(w)?;

    walk_node(nar_root_node, proto_root_node, store_client)
}

/// Process an intermediate node in the structure.
/// This consumes the node.
fn walk_node<SC: StoreClient>(
    nar_node: tvix_nar::Node,
    proto_node: proto::node::Node,
    store_client: &mut SC,
) -> Result<()> {
    match proto_node {
        proto::node::Node::Symlink(proto_symlink_node) => {
            nar_node.symlink(&proto_symlink_node.target)?;
        }
        proto::node::Node::File(proto_file_node) => {
            nar_node.file(
                proto_file_node.executable,
                proto_file_node.size.into(),
                &mut store_client.open_blob(proto_file_node.digest)?,
            )?;
        }
        proto::node::Node::Directory(proto_directory_node) => {
            // look up that node from the store client
            let proto_directory = store_client.get_directory(proto_directory_node.digest)?;

            // if it's None, that's an error!
            if proto_directory.is_none() {
                // TODO: proper error handling
                panic!("not found!")
            }

            // start a directory node
            let mut nar_node_directory = nar_node.directory()?;

            // for each node in the directory, create a new entry with its name,
            // and then invoke walk_node on that entry.
            for proto_node in proto_directory.unwrap().nodes() {
                let child_node = nar_node_directory.entry(proto_node.get_name())?;
                walk_node(child_node, proto_node, store_client)?;
            }

            // close the directory
            nar_node_directory.close()?;
        }
    }
    Ok(())
}
