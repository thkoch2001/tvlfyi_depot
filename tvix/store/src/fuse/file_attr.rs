use std::mem::MaybeUninit;

use super::inodes::{DirectoryInodeData, InodeData};
use const_zero::const_zero;

/// The [FileAttr] describing the root
pub const ROOT_FILE_ATTR: libc::stat64 = get_root_file_attr();

const fn get_root_file_attr() -> libc::stat64 {
    // TODO: replace `const_zero` once [std::mem::MaybeUninit::zeroed] in const is stabilized.
    let mut stat64: libc::stat64 = unsafe { const_zero!(libc::stat64) };
    stat64.st_ino = fuse_backend_rs::api::filesystem::ROOT_ID;
    stat64.st_size = 0;
    stat64.st_blocks = 1024;
    stat64.st_mode = libc::S_IFDIR | 0o555;
    stat64
}

/// for given &Node and inode, construct a [libc::stat64]
pub fn gen_file_attr(inode_data: &InodeData, inode: u64) -> libc::stat64 {
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
