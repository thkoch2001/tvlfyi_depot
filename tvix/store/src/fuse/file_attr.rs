use std::time::SystemTime;

use crate::proto::node::Node;
use fuser::{FileAttr, FileType};

/// for given &Node and inode, construct a [FileAttr]
pub fn gen_file_attr(node: &Node, inode: u64) -> FileAttr {
    FileAttr {
        ino: inode,
        size: match node {
            Node::Directory(directory_node) => directory_node.size as u64,
            Node::File(file_node) => file_node.size as u64,
            Node::Symlink(symlink_node) => symlink_node.target.len() as u64, // trololo
        },
        // TODO: play with this numbers. Does it affect read sizes?
        blksize: 1024,
        blocks: 0,
        atime: SystemTime::UNIX_EPOCH,
        mtime: SystemTime::UNIX_EPOCH,
        ctime: SystemTime::UNIX_EPOCH,
        crtime: SystemTime::UNIX_EPOCH,
        kind: match node {
            Node::Directory(_) => FileType::Directory,
            Node::File(_) => FileType::RegularFile,
            Node::Symlink(_) => FileType::Symlink,
        },
        perm: match node {
            Node::Directory(_) => 0o555,
            Node::File(_) => 0o444,
            Node::Symlink(_) => 0o444,
        },
        nlink: 0,
        uid: 0,
        gid: 0,
        rdev: 0,
        flags: 0,
    }
}
