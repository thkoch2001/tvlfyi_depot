use std::time::SystemTime;

use super::inodes::InodeData;
use fuser::FileAttr;

/// for given &Node and inode, construct a [FileAttr]
pub fn gen_file_attr(node: &InodeData, inode: u64) -> FileAttr {
    FileAttr {
        ino: inode,
        size: match node {
            InodeData::Regular(_, size, _) => *size as u64,
            InodeData::Symlink(target) => target.len() as u64,
            InodeData::Directory(_, ref children) => {
                children.as_ref().expect("must be some").len() as u64 + 1
            }
        },
        // TODO: play with this numbers. Does it affect read sizes?
        blksize: 1024,
        blocks: 0,
        atime: SystemTime::UNIX_EPOCH,
        mtime: SystemTime::UNIX_EPOCH,
        ctime: SystemTime::UNIX_EPOCH,
        crtime: SystemTime::UNIX_EPOCH,
        kind: node.into(),
        perm: match node {
            InodeData::Regular(..) => 0o444,
            InodeData::Symlink(_) => 0o444,
            InodeData::Directory(..) => 0o555,
        },
        nlink: 0,
        uid: 0,
        gid: 0,
        rdev: 0,
        flags: 0,
    }
}
