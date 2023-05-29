mod file_attr;
mod inodes;
use crate::{
    blobservice::BlobService,
    directoryservice::DirectoryService,
    fuse::{file_attr::gen_file_attr, inodes::InodeData},
    pathinfoservice::PathInfoService,
    proto::{node::Node, NamedNode},
    B3Digest, Error,
};
use fuser::{FileAttr, ReplyAttr, Request};
use nix_compat::store_path::StorePath;
use std::io::Read;
use std::{collections::HashMap, time::Duration};
use std::{rc::Rc, sync::Arc};
use tracing::{debug, info_span, warn};

use self::inodes::InodeTracker;

/// This implements a read-only FUSE filesystem for a tvix-store
/// with the passed [BlobService], [DirectoryService] and [PathInfoService].
///
/// We don't allow listing on the root mountpoint (inode 0).
/// In the future, this might be made configurable once a listing method is
/// added to [self.path_info_service], and then show all store paths in that
/// store.
///
/// Linux uses inodes in filesystems. When implementing FUSE, most calls are
/// *for* a given inode, and preceded by a `lookup` call with the parent inode
/// and the name that's being looked up.
///
/// This means, we need to have a stable mapping of inode numbers to the
/// corresponding store nodes.
///
/// We internally delegate all inode allocation and state keeping to a
/// [InodeTracker], and store the currently "explored" store paths together with
/// root inode of the root.
///
/// There's only one place where inodes are allocated / data inserted into
/// [self.inode_tracker], if not allocated before already:
/// When proecessing a `lookup`, either in the mount root for a not-yet explored
/// store path, or while looking at a node which is a not-yet explored Directory.
///
///  Things pointing to the same contents get the same inodes, irrespective of
///  their own location.
///  This means:
///  - Symlinks with the same target will get the same inode.
///  - Regular/executable files with the same contents will get the same inode
///  - Directories with the same contents will get the same inode.
///
/// Due to the above being valid across the whole store, and considering the
/// merkle structure is a DAG, not a tree, this also means we can't do "bucketed
/// allocation", aka reserve Directory.size inodes for each PathInfo.
pub struct FUSE {
    blob_service: Arc<dyn BlobService>,
    directory_service: Arc<dyn DirectoryService>,
    path_info_service: Arc<dyn PathInfoService>,

    /// This maps a given StorePath to the inode we allocated for the root inode.
    store_paths: HashMap<StorePath, u64>,

    /// This keeps track of inodes and data alongside them.
    inode_tracker: InodeTracker,
}

impl FUSE {
    pub fn new(
        blob_service: Arc<dyn BlobService>,
        directory_service: Arc<dyn DirectoryService>,
        path_info_service: Arc<dyn PathInfoService>,
    ) -> Self {
        Self {
            blob_service,
            directory_service,
            path_info_service,

            store_paths: HashMap::default(),
            inode_tracker: Default::default(),
        }
    }

    /// This will turn a lookup request for [std::ffi::OsStr] in the root to
    /// a ino and [InodeData].
    /// It will peek in [self.store_paths], and then either look it up from
    /// [self.inode_tracker],
    /// or otherwise fetch from [self.path_info_service], and then insert into
    /// [self.inode_tracker].
    fn name_in_root_to_ino_and_data(
        &mut self,
        name: &std::ffi::OsStr,
    ) -> Result<Option<(u64, Rc<InodeData>)>, Error> {
        // parse the name into a [StorePath].
        let store_path = if let Some(name) = name.to_str() {
            match StorePath::from_string(name) {
                Ok(store_path) => store_path,
                Err(e) => {
                    debug!(e=?e, "unable to parse as store path");
                    // This is not an error, but a "ENOENT", as someone can stat
                    // a file inside the root that's no valid store path
                    return Ok(None);
                }
            }
        } else {
            debug!("{name:?} is no string");
            // same here.
            return Ok(None);
        };

        if let Some(ino) = self.store_paths.get(&store_path) {
            // If we already have that store path, lookup the inode from
            // self.store_paths and then get the data from [self.inode_tracker],
            // which in the case of a [InodeData::Directory] will be fully
            // populated.
            Ok(Some((
                *ino,
                self.inode_tracker.get(*ino).expect("must exist"),
            )))
        } else {
            // If we don't have it, look it up in PathInfoService.
            match self.path_info_service.get(store_path.digest)? {
                // the pathinfo doesn't exist, so the file doesn't exist.
                None => Ok(None),
                Some(path_info) => {
                    // The pathinfo does exist, construct [InodeData] for the root node.
                    // If this is a [Node::Directory], [self.node_to_inode_data]
                    // will fetch the Directory object from the
                    // DirectoryService.
                    let root_node = path_info.node.unwrap().node.unwrap();
                    let inode_data = self.node_to_inode_data(root_node)?;

                    // add to inode tracker and register in self.store_paths.
                    let ino = self.inode_tracker.put(inode_data);
                    self.store_paths.insert(store_path, ino);

                    // FUTUREWORK: change put to return the data after
                    // inserting, so we don't need to lookup a second
                    // time?
                    Ok(Some((ino, self.inode_tracker.get(ino).unwrap())))
                }
            }
        }
    }

    /// This will turn a Node to a InodeData.
    /// In case it's a directory, its details will be looked up from the
    /// [DirectoryService].
    /// This is both used to initially insert the root node of a store path,
    /// as well as when looking up an intermediate DirectoryNode.
    fn node_to_inode_data(&self, node: Node) -> Result<InodeData, Error> {
        match node {
            Node::Directory(crate::proto::DirectoryNode { digest, .. }) => {
                // if the node is a directory, look it up from DirectoryService.
                let directory_digest = B3Digest::from_vec(digest).unwrap();
                match self.directory_service.get(&directory_digest) {
                    Err(e) => {
                        warn!(e = e.to_string(), directory.digest=%directory_digest, "failed to get directory");
                        Err(e)
                    }
                    // If the Directory can't be found, this is a hole, bail out.
                    Ok(None) => {
                        tracing::error!(directory.digest=%directory_digest, "directory not found in directory service");
                        Err(Error::StorageError(format!(
                            "directory {} not found",
                            directory_digest
                        )))
                    }
                    Ok(Some(directory)) => Ok(directory.into()),
                }
            }
            Node::File(crate::proto::FileNode {
                digest,
                size,
                executable,
                ..
            }) => Ok(InodeData::Regular(
                B3Digest::from_vec(digest).unwrap(),
                size,
                executable,
            )),
            Node::Symlink(crate::proto::SymlinkNode { target, .. }) => {
                Ok(InodeData::Symlink(target))
            }
        }
    }
}

impl fuser::Filesystem for FUSE {
    #[tracing::instrument(skip_all, fields(rq.inode = ino))]
    fn getattr(&mut self, _req: &Request, ino: u64, reply: ReplyAttr) {
        debug!("getattr");

        if ino == fuser::FUSE_ROOT_ID {
            reply.attr(
                &Duration::MAX,
                // TODO: ugly. do we want to just have that one in a constant?
                &file_attr::gen_file_attr(
                    &inodes::InodeData::Directory(
                        B3Digest::from_vec(vec![
                            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
                            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
                            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
                        ])
                        .unwrap(),
                        None,
                    ),
                    ino,
                ),
            );
            return;
        }

        match self.inode_tracker.get(ino) {
            None => reply.error(libc::ENOENT),
            Some(node) => {
                debug!(node = ?node, "found node");
                reply.attr(&Duration::MAX, &file_attr::gen_file_attr(&node, ino));
            }
        }
    }

    #[tracing::instrument(skip_all, fields(rq.parent_inode = parent_ino, rq.name = ?name))]
    fn lookup(
        &mut self,
        _req: &Request,
        parent_ino: u64,
        name: &std::ffi::OsStr,
        reply: fuser::ReplyEntry,
    ) {
        debug!("lookup");

        // This goes from a parent inode to a node.
        // - If the parent is [fuser::FUSE_ROOT_ID], we need to check
        //   [self.store_paths] (fetching from PathInfoService if needed)
        // - Otherwise, lookup the parent in [self.inode_tracker] (which must be
        //   a [InodeData::Directory]), and find the child with that name.
        if parent_ino == fuser::FUSE_ROOT_ID {
            match self.name_in_root_to_ino_and_data(name) {
                Err(_e) => {
                    reply.error(libc::ENOSYS);
                }
                Ok(None) => {
                    reply.error(libc::ENOENT);
                }
                Ok(Some((ino, inode_data))) => {
                    reply_with_entry(reply, Ok(Some(&gen_file_attr(&inode_data, ino))));
                }
            }
        } else {
            // This is the "lookup for "a" inside inode 42.
            // We lookup the parent directory listing, which must exist.
            // In there, we search for the name and return the attr.
            // In case the node is a directory, it calls get_directory_listing
            // to get the children populated.

            // this must already exist.
            let parent_data = self.inode_tracker.get(parent_ino).unwrap();
            match *parent_data {
                InodeData::Regular(..) | InodeData::Symlink(_) => {
                    // if the parent inode was not a directory, this doesn't make sense
                    reply.error(libc::ENOSYS);
                }
                InodeData::Directory(ref _directory_digest, ref children) => {
                    // the children must already be populated during the lookup of that parent.
                    let children = children.as_ref().unwrap();

                    // in the children, find the one with the desired name.
                    if let Some((child_ino, _)) = children.iter().find(|e| e.1.get_name() == name) {
                        // lookup the child [InodeData] in [self.inode_tracker].
                        let mut child_inode_data = self.inode_tracker.get(*child_ino).unwrap();

                        // Reply with the file attributes, except for when this
                        // is an unpopulated Directory, in which case we fetch
                        // it from [self.directory_service] first.
                        match *child_inode_data {
                            InodeData::Regular(..) => {}
                            InodeData::Symlink(_) => {}
                            InodeData::Directory(ref directory_digest, ref children) => {
                                if children.is_none() {
                                    // Populate. This function only cares about the digest,
                                    // so it's fine for the other values to be bogus.
                                    // FUTUREWORK: change the types that
                                    // node_to_inode_data interacts with, maybe
                                    // InodeData as input too?
                                    let inode_data = self.node_to_inode_data(
                                        crate::proto::node::Node::Directory(
                                            crate::proto::DirectoryNode {
                                                name: "".to_string(),
                                                size: 0,
                                                digest: directory_digest.to_vec(),
                                            },
                                        ),
                                    );

                                    match inode_data {
                                        Err(_) => {
                                            reply.error(libc::EINVAL);
                                            return;
                                        }
                                        Ok(inode_data) => {
                                            // add to inode tracker.
                                            let ino = self.inode_tracker.put(inode_data);
                                            debug_assert_eq!(*child_ino, ino);

                                            // mutate child_inode_data from above with the augmented data.
                                            child_inode_data =
                                                self.inode_tracker.get(*child_ino).unwrap();
                                        }
                                    };
                                }
                            }
                        }

                        reply_with_entry(
                            reply,
                            Ok(Some(&gen_file_attr(&child_inode_data, *child_ino))),
                        );
                    } else {
                        // Child not found, return ENOENT.
                        reply.error(libc::ENOENT);
                    }
                }
            }
        }
    }

    // TODO: readdirplus?

    #[tracing::instrument(skip_all, fields(rq.inode = ino, rq.offset = offset))]
    fn readdir(
        &mut self,
        _req: &Request<'_>,
        ino: u64,
        _fh: u64,
        offset: i64,
        mut reply: fuser::ReplyDirectory,
    ) {
        debug!("readdir");

        if ino == fuser::FUSE_ROOT_ID {
            reply.error(libc::ENOSYS);
            return;
        }

        // lookup the node by inode. We rely on the previous `lookup` populating everything.
        match *self.inode_tracker.get(ino).unwrap() {
            InodeData::Regular(..) | InodeData::Symlink(..) => {
                warn!("invalid readdir");
                reply.error(libc::ENOSYS);
            }
            InodeData::Directory(ref directory_digest, None) => {
                panic!("children not popuplated in directory {}", directory_digest);
            }
            InodeData::Directory(ref _directory_digest, Some(ref children)) => {
                for (i, (ino, child_node)) in children.iter().skip(offset as usize).enumerate() {
                    // the second parameter will become the "offset" parameter on the next call.
                    let full = reply.add(
                        *ino,
                        offset + i as i64 + 1_i64,
                        match child_node {
                            Node::Directory(_) => fuser::FileType::Directory,
                            Node::File(_) => fuser::FileType::RegularFile,
                            Node::Symlink(_) => fuser::FileType::Symlink,
                        },
                        child_node.get_name(),
                    );
                    if full {
                        break;
                    }
                }
                reply.ok();
            }
        }
    }

    /// TODO: implement open + close?

    #[tracing::instrument(skip_all, fields(rq.inode = ino, rq.offset = offset, rq.size = size))]
    fn read(
        &mut self,
        _req: &Request<'_>,
        ino: u64,
        _fh: u64,
        offset: i64,
        size: u32,
        _flags: i32,
        _lock_owner: Option<u64>,
        reply: fuser::ReplyData,
    ) {
        debug!("read");

        if ino == fuser::FUSE_ROOT_ID {
            reply.error(libc::ENOSYS);
            return;
        }
        // lookup the inode
        match *self.inode_tracker.get(ino).unwrap() {
            // read is invalid on non-files.
            InodeData::Directory(..) | InodeData::Symlink(_) => {
                reply.error(libc::ENOSYS);
            }
            InodeData::Regular(ref blob_digest, _blob_size, _) => {
                let span = info_span!("read", blob.digest = %blob_digest);
                let _enter = span.enter();

                match self.blob_service.open_read(blob_digest) {
                    Ok(None) => {
                        warn!("blob not found");
                        reply.error(libc::ENOSYS);
                    }
                    Err(e) => {
                        warn!(e=?e, "error opening blob");
                        reply.error(libc::ENOSYS);
                    }
                    Ok(Some(blob_reader)) => {
                        let data: std::io::Result<Vec<u8>> = blob_reader
                            .bytes()
                            // TODO: this is obviously terrible. blobreader should implement seek.
                            .skip(offset.try_into().unwrap())
                            .take(size.try_into().unwrap())
                            .collect();

                        match data {
                            Ok(data) => {
                                // respond with the requested data
                                reply.data(&data);
                            }
                            Err(e) => reply.error(e.raw_os_error().unwrap()),
                        }
                    }
                }
            }
        }
    }

    #[tracing::instrument(skip_all, fields(rq.inode = ino))]
    fn readlink(&mut self, _req: &Request<'_>, ino: u64, reply: fuser::ReplyData) {
        if ino == fuser::FUSE_ROOT_ID {
            reply.error(libc::ENOSYS);
            return;
        }

        // lookup the inode
        match *self.inode_tracker.get(ino).unwrap() {
            InodeData::Directory(..) | InodeData::Regular(..) => {
                reply.error(libc::ENOSYS);
            }
            InodeData::Symlink(ref target) => reply.data(target.as_bytes()),
        }
    }
}

fn reply_with_entry(reply: fuser::ReplyEntry, file_attr: Result<Option<&FileAttr>, crate::Error>) {
    match file_attr {
        Err(e) => {
            warn!(e = %e);
            reply.error(libc::EINVAL)
        }
        Ok(None) => reply.error(libc::ENOENT),
        Ok(Some(file_attr)) => {
            reply.entry(&Duration::MAX, file_attr, 1 /* TODO: generation */);
        }
    }
}
