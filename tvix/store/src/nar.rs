//! This provides some common "client-side" libraries to interact with a tvix-
//! store, in this case to render NAR.
use anyhow::Result;

use crate::proto;
use crate::proto::Directory;

pub trait Blob: std::io::BufRead /* + std::io::Seek */ {}

pub trait StoreClient {
    fn open_blob(&self, digest: Vec<u8>) -> std::io::Result<Box<dyn Blob>>;

    // TODO: stat_blob, put_blob?
    fn get_directory(&self, digest: Vec<u8>) -> std::io::Result<Option<Directory>>;

    // TODO: put_directory
}

#[cfg(test)]
mod tests {
    use data_encoding::BASE64;

    use super::*;

    #[derive(Default)]
    struct FailingStoreClient {}

    impl StoreClient for FailingStoreClient {
        fn open_blob(&self, digest: Vec<u8>) -> std::io::Result<Box<dyn Blob>> {
            panic!(
                "open_blob should never be called, but was called with {}",
                BASE64.encode(&digest),
            );
        }

        fn get_directory(&self, digest: Vec<u8>) -> std::io::Result<Option<Directory>> {
            panic!(
                "get_directory should never be called, but was called with {}",
                BASE64.encode(&digest),
            );
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
            &mut store_client,
        )
        .expect("must succeed");

        assert_eq!(
            buf,
            vec![
                13, 0, 0, 0, 0, 0, 0, 0, // 13 bytes of data
                110, 105, 120, 45, 97, 114, 99, 104, 105, 118, 101, 45, 49, 0, 0,
                0, // "nix-archive-1" (plus padding)
                1, 0, 0, 0, 0, 0, 0, 0, // 1 byte of data
                40, 0, 0, 0, 0, 0, 0, 0, // "("
                4, 0, 0, 0, 0, 0, 0, 0, // 4 bytes of data
                116, 121, 112, 101, 0, 0, 0, 0, // "type"
                7, 0, 0, 0, 0, 0, 0, 0, // 7 bytes of data
                115, 121, 109, 108, 105, 110, 107, 0, // "symlink"
                6, 0, 0, 0, 0, 0, 0, 0, // 6 bytes of data
                116, 97, 114, 103, 101, 116, 0, 0, // target
                24, 0, 0, 0, 0, 0, 0, 0, // 24 bytes of data
                47, 110, 105, 120, 47, 115, 116, 111, 114, 101, 47, 115, 111, 109, 101, 119, 104,
                101, 114, 101, 101, 108, 115, 101, // "/nix/store/somewhereelse"
                1, 0, 0, 0, 0, 0, 0, 0, // 1 byte of data
                41, 0, 0, 0, 0, 0, 0, 0 // ")"
            ]
        );
        Ok(())
    }
}

/// Uses the root node, and optionally clients to the directory and blob
/// service to assemble a NAR file, and write its contents to the passed writer.
pub fn write_nar<W: std::io::Write, SC: StoreClient>(
    w: &mut W,
    proto_root_node: crate::proto::node::Node,
    store_client: &mut SC,
) -> Result<()> {
    // Initialize tvix_nar
    let nar_root_node = tvix_nar::open(w)?;

    walk_node(nar_root_node, proto_root_node, store_client)
}

fn walk_node<SC: StoreClient>(
    nar_node: tvix_nar::Node,
    proto_node: proto::node::Node,
    store_client: &mut SC,
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
        proto::node::Node::Directory(_directory_node) => {
            // ask the directory_service_client for all directory messages
            // (we can use recursive here and put all in a HashSet)

            let _nar_node_directory = nar_node.directory()?;

            // enumerate over the three different lists directory_node, pick the smallest one
            // TODO: do we want an iterator for that on Directory, returning proto::node::Node ?

            // for each of these, invoke
            // nar_node_directory.entry("the_name") and pass the result along to walk_node

            Ok(())
        }
    }
}
