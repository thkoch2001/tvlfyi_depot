use std::{mem::MaybeUninit, time::SystemTime};

use super::inodes::{DirectoryInodeData, InodeData};
use const_zero::const_zero;
use fuser::FileAttr;

/// The [FileAttr] describing the root
pub const ROOT_FILE_ATTR: FileAttr = FileAttr {
    ino: fuser::FUSE_ROOT_ID,
    size: 0,
    blksize: 1024,
    blocks: 0,
    atime: SystemTime::UNIX_EPOCH,
    mtime: SystemTime::UNIX_EPOCH,
    ctime: SystemTime::UNIX_EPOCH,
    crtime: SystemTime::UNIX_EPOCH,
    kind: fuser::FileType::Directory,
    perm: 0o555,
    nlink: 0,
    uid: 0,
    gid: 0,
    rdev: 0,
    flags: 0,
};

pub const ROOT_FILE_ATTR_LIBC: libc::stat64 = get_root_file_attr();

const fn get_root_file_attr() -> libc::stat64 {
    let mut stat64: libc::stat64 = unsafe { const_zero!(libc::stat64) };
    stat64.st_ino = fuse_backend_rs::api::filesystem::ROOT_ID;
    stat64.st_size = 0;
    stat64.st_blocks = 1024;
    stat64.st_mode = libc::S_IFDIR | 0o555;
    stat64
}

/// for given &Node and inode, construct a [FileAttr]
pub fn gen_file_attr_libc(inode_data: &InodeData, inode: u64) -> libc::stat64 {
    let mut stat64: libc::stat64 = unsafe { MaybeUninit::zeroed().assume_init() };

    stat64.st_ino = inode;
    stat64.st_size = match inode_data {
        InodeData::Regular(_, size, _) => *size as i64,
        InodeData::Symlink(target) => target.len() as i64,
        InodeData::Directory(DirectoryInodeData::Sparse(_, size)) => *size as i64,
        InodeData::Directory(DirectoryInodeData::Populated(_, ref children)) => {
            children.len() as i64
        }
    };
    // FUTUREWORK: play with this numbers, as it affects read sizes for client applications.
    stat64.st_blocks = 1024;
    stat64.st_mode = match inode_data {
        InodeData::Regular(_, _, _) => libc::S_IFREG | 0o444,
        InodeData::Symlink(_) => libc::S_IFLNK | 0o444,
        InodeData::Directory(_) => libc::S_IFDIR | 0o555,
    };

    stat64
}

/// for given &Node and inode, construct a [FileAttr]
pub fn gen_file_attr(inode_data: &InodeData, inode: u64) -> FileAttr {
    FileAttr {
        ino: inode,
        size: match inode_data {
            InodeData::Regular(_, size, _) => *size as u64,
            InodeData::Symlink(target) => target.len() as u64,
            InodeData::Directory(DirectoryInodeData::Sparse(_, size)) => *size as u64,
            InodeData::Directory(DirectoryInodeData::Populated(_, ref children)) => {
                children.len() as u64
            }
        },
        // FUTUREWORK: play with this numbers, as it affects read sizes for client applications.
        blksize: 1024,
        blocks: 0,
        atime: SystemTime::UNIX_EPOCH,
        mtime: SystemTime::UNIX_EPOCH,
        ctime: SystemTime::UNIX_EPOCH,
        crtime: SystemTime::UNIX_EPOCH,
        kind: inode_data.into(),
        perm: match inode_data {
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
