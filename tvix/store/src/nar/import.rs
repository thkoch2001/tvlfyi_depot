use std::{
    io::{self, Read},
    sync::Arc,
};

use bytes::Bytes;
use nix_compat::nar::reader::Node;
use tokio_util::io::SyncIoBridge;
use tracing::warn;
use tvix_castore::{
    blobservice::BlobService,
    directoryservice::{DirectoryPutter, DirectoryService},
    proto::{self as castorepb},
};

/// Accepts a reader providing a NAR.
/// Will traverse it, uploading blobs to the given [BlobService], and
/// directories to the given [DirectoryService].
/// On success, the root node is returned.
/// This function is not async (because the NAR reader is not)
/// and calls [tokio::task::block_in_place] when interacting with backing
/// services, so make sure to only call this with spawn_blocking.
pub fn read_nar<R: Read + Send>(
    r: &mut R,
    blob_service: Arc<dyn BlobService>,
    directory_service: Arc<dyn DirectoryService>,
) -> io::Result<castorepb::node::Node> {
    let handle = tokio::runtime::Handle::current();

    let directory_putter = directory_service.put_multiple_start();

    let node = nix_compat::nar::reader::open(r)?;
    let (root_node, mut directory_putter) = process_node(
        handle.clone(),
        "".into(),
        node,
        blob_service,
        directory_putter,
    )?;

    // Close directory_putter to make sure all directories have been inserted.
    let resp = handle.block_on(handle.spawn(async move { directory_putter.close().await }))?;

    // TODO: DirectoryPutter should be refactored, it shouldn't emit an error if
    // we don't didn't end up inserting anything.
    if let Err(e) = resp {
        match root_node {
            castorepb::node::Node::Directory(_) => {
                warn!("failed to close directory putter: {:?}", e);
                return Err(e.into());
            }
            _ => {}
        }
    }

    Ok(root_node)
}

fn process_node(
    handle: tokio::runtime::Handle,
    name: bytes::Bytes,
    node: Node,
    blob_service: Arc<dyn BlobService>,
    directory_putter: Box<dyn DirectoryPutter>,
) -> io::Result<(tvix_castore::proto::node::Node, Box<dyn DirectoryPutter>)> {
    Ok((
        match node {
            Node::Symlink { target } => castorepb::node::Node::Symlink(castorepb::SymlinkNode {
                name,
                target: target.into(),
            }),
            Node::File {
                executable,
                mut reader,
            } => castorepb::node::Node::File(process_file_node(
                handle,
                name,
                executable,
                &mut reader,
                blob_service,
            )?),
            Node::Directory(mut dir_reader) => {
                let (node, directory_putter) = process_directory_node(
                    handle,
                    name,
                    &mut dir_reader,
                    blob_service.clone(),
                    directory_putter,
                )?;

                return Ok((castorepb::node::Node::Directory(node), directory_putter));
            }
        },
        directory_putter,
    ))
}

fn process_file_node(
    handle: tokio::runtime::Handle,
    name: Bytes,
    executable: bool,
    reader: &mut nix_compat::nar::reader::FileReader,
    blob_service: Arc<dyn BlobService>,
) -> io::Result<castorepb::FileNode> {
    // store the length. If we read any other length, reading will fail.
    let expected_len = reader.len();

    // prepare writing a new blob.
    let blob_writer = {
        let task = handle.spawn({
            let blob_service = blob_service.clone();
            async move { blob_service.open_write().await }
        });
        handle.block_on(task)?
    };

    // write the blob.
    let mut blob_writer = {
        let mut dest = SyncIoBridge::new(blob_writer);
        io::copy(reader, &mut dest)?;

        dest.shutdown()?;

        // return back the blob_reader
        dest.into_inner()
    };

    // close the blob_writer, retrieve the digest.
    let blob_digest = {
        let task = handle.spawn(async move { blob_writer.close().await });
        handle.block_on(task)??
    };

    Ok(castorepb::FileNode {
        name,
        digest: blob_digest.into(),
        size: expected_len as u32, // TODO: check overflow?
        executable,
    })
}

fn process_directory_node(
    handle: tokio::runtime::Handle,
    name: Bytes,
    dir_reader: &mut nix_compat::nar::reader::DirReader,
    blob_service: Arc<dyn BlobService>,
    directory_putter: Box<dyn DirectoryPutter>,
) -> io::Result<(castorepb::DirectoryNode, Box<dyn DirectoryPutter>)> {
    let mut directory = castorepb::Directory::default();

    let mut directory_putter = directory_putter;
    while let Some(entry) = dir_reader.next()? {
        let (node, directory_putter_back) = process_node(
            handle.clone(),
            entry.name.into(),
            entry.node,
            blob_service.clone(),
            directory_putter,
        )?;

        directory_putter = directory_putter_back;

        match node {
            castorepb::node::Node::Directory(node) => directory.directories.push(node),
            castorepb::node::Node::File(node) => directory.files.push(node),
            castorepb::node::Node::Symlink(node) => directory.symlinks.push(node),
        }
    }

    // upload the directory.
    let (directory_putter, directory_digest, directory_size) = {
        let digest = directory.digest();
        let size = directory.size();
        let task = handle.spawn(async move {
            directory_putter.put(directory).await?;
            Ok::<_, io::Error>(directory_putter)
        });

        let directory_putter = handle.block_on(task)??;

        (directory_putter, digest, size)
    };

    Ok((
        castorepb::DirectoryNode {
            name,
            digest: directory_digest.into(),
            size: directory_size,
        },
        directory_putter,
    ))
}
