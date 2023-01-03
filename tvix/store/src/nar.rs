//! This provides some common "client-side" libraries to interact with a tvix-
//! store, in this case to render NAR.
use anyhow::Result;

use crate::proto;
use crate::proto::Directory;

pub trait Blob: std::io::BufRead /* + std::io::Seek */ {}

pub trait StoreClient<B: Blob> {
    fn open_blob(&self, digest: Vec<u8>) -> std::io::Result<B>;

    // TODO: stat_blob, put_blob?
    fn get_directory(&self, digest: Vec<u8>) -> std::io::Result<Option<Directory>>;

    // TODO: put_directory
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Default)]
    struct FailingStoreClient {}

    impl StoreClient<B: Blob> for FailingStoreClient {
        fn open_blob(&self, digest: Vec<u8>) -> std::io::Result<dyn Blob> {
            panic!();
        }

        fn get_directory(&self, digest: Vec<u8>) -> std::io::Result<Option<Directory>> {
            panic!();
        }
    }

    #[tokio::test]
    async fn single_symlink() -> anyhow::Result<()> {
        let mut buf: Vec<u8> = vec![];
        let mut store_client = FailingStoreClient::default();

        write_nar(
            &mut buf,
            crate::proto::node::Node::Symlink(crate::proto::SymlinkNode {
                name: "doesntmatter".to_string(),
                target: "/nix/store/somewhereelse".to_string(),
            }),
            &store_client,
        )
        .expect("must succeed");

        assert_eq!(buf, vec![0x01]);
    }
}

/// Uses the root node, and optionally clients to the directory and blob
/// service to assemble a NAR file, and write its contents to the passed writer.
pub fn write_nar<W: std::io::Write, SC: StoreClient<B>, B: Blob>(
    w: &mut W,
    proto_root_node: crate::proto::node::Node,
    store_client: SC,
) -> Result<()> {
    // Initialize tvix_nar
    let nar_root_node = tvix_nar::open(w)?;

    walk_node(nar_root_node, proto_root_node, store_client)
}

fn walk_node<SC: StoreClient<B>, B: Blob>(
    nar_node: tvix_nar::Node,
    proto_node: proto::node::Node,
    store_client: SC,
) -> Result<()> {
    match proto_node {
        proto::node::Node::Symlink(symlink_node) => {
            nar_node.symlink(&symlink_node.target)?;

            Ok(())
        }
        proto::node::Node::File(file_node) => {
            nar_node.file(
                file_node.executable,
                file_node.size.into(),
                &mut store_client.open_blob(file_node.digest)?,
            )?;

            Ok(())
        }
        proto::node::Node::Directory(directory_node) => {
            // ask the directory_service_client for all directory messages
            // (we can use recursive here and put all in a HashSet)

            let nar_node_directory = nar_node.directory()?;

            // enumerate over the three different lists directory_node, pick the smallest one
            // TODO: do we want an iterator for that on Directory, returning proto::node::Node ?

            // for each of these, invoke
            // nar_node_directory.entry("the_name") and pass the result along to walk_node

            Ok(())
        }
    }
}
