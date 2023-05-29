mod file_attr;
use crate::{
    blobservice::BlobService,
    directoryservice::DirectoryService,
    fuse::file_attr::gen_file_attr,
    pathinfoservice::PathInfoService,
    proto::{node::Node, DirectoryNode, NamedNode},
    B3Digest,
};
use fuser::{FileAttr, FileType, ReplyAttr, Request};
use nix_compat::store_path::StorePath;
use std::sync::Arc;
use std::{
    collections::{BTreeMap, HashMap},
    ffi::OsString,
    io::Read,
    rc::Rc,
    time::Duration,
};
use tracing::{debug, info_span, instrument, warn};

type DirectoryListing = BTreeMap<OsString, (u64, fuser::FileType)>;

/// This implements a read-only FUSE filesystem for a tvix-store
/// with the passed [BlobService], [DirectoryService] and [PathInfoService].
///
/// We don't allow listing on the root mountpoint (inode 0).
/// In the future, this might be made configurable once a listing method is
/// added.
///
/// Linux uses inodes in filesystems - a stat call is *for* a given inode.
/// This means, we need to have a stable mapping of tvix-store nodes to their
/// inodes.
///
/// In a tvix-store FUSE filesystem, there's two places where inodes can be
/// allocated:
///  - When receiving a `stat` request in the root, which maps to a `PathInfo` /
///    valid store path.
///  - When listing a directory (whose inode is already known) and listing new
///    children.
///
///  Things pointing to the same contents get the same inodes, irrespective of
///  their own location.
///  This means:
///  - Symlinks with the same target will get the same inode.
///  - Regular/executable files with the same contents will get the same inode
///  - Directories with the same contents will get the same inode.
///    TODO: check our listing method does this, for all three types!
///
/// Due to the above, and considering the merkle structure is a DAG, not
/// a tree, this also means we can't do "bucketed allocation", aka reserve
/// Directory.size inodes for each PathInfo. The above invariant works *across*
/// different store paths.
///
/// We internally provide three internal state lookup HashMaps:
///
/// `store_paths` maps from a given [StorePath] to the inode that has been
/// allocated for the root node (which can also be shared with other nodes, both
/// root or intermediate).
/// `inodes` points from an inode to a `Node`, and contains all inodes.
/// `directories`
/// TODO: `directories`. `DirectoryListing` is ugly. Should probably be
/// refactored to query DirectoryService, and we rely on store composition to make this fast.
/// However, we need to make sure we allocate inodes properly when getting things back,
/// which means we might still need to keep some inode lookup tables locally. Meh.
pub struct FUSE {
    blob_service: Arc<dyn BlobService>,
    directory_service: Arc<dyn DirectoryService>,
    path_info_service: Arc<dyn PathInfoService>,

    /// This maps a given StorePath to the inode we allocated for the root inode.
    store_paths: HashMap<StorePath, u64>,

    /// This holds all nodes (including the ones in [store_paths]), keyed by their inode.
    inodes: HashMap<u64, Node>,

    /// This maps from a directory digest to all elements (name, inode, FileType).
    directories: HashMap<B3Digest, Rc<DirectoryListing>>,

    next_inode: u64,
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
            inodes: HashMap::default(),
            directories: HashMap::default(),
            next_inode: 2, // 1 is reserved for the root
        }
    }

    /// For given directory_digest, this will return a `Rc<DirectoryListing>` for it.
    /// It'll populate a cache for it in self.directories.
    #[instrument(skip_all,fields(directory.digest=%directory_digest),err)]
    fn get_directory_listing(
        &mut self,
        ino: u64,
        parent: u64,
        directory_digest: &B3Digest,
    ) -> Result<Rc<DirectoryListing>, crate::Error> {
        debug!("get_directory_listing");
        if let Some(directory_listing) = self.directories.get(directory_digest) {
            Ok(directory_listing.clone())
        } else {
            // If the list of directories is not populated yet,
            // look it up from DirectoryService.
            match self.directory_service.get(directory_digest)? {
                None => {
                    // This is a hole, shouldn't happen!
                    Err(crate::Error::StorageError(
                        "directory not found".to_string(),
                    ))
                }
                Some(directory) => {
                    // allocate inodes and put it into self.directories.
                    let mut directory_listing: DirectoryListing = BTreeMap::new();

                    directory_listing.insert(".".into(), (ino, fuser::FileType::Directory));
                    directory_listing.insert("..".into(), (parent, fuser::FileType::Directory));

                    for node in directory.nodes() {
                        let ino = self.next_inode;
                        self.inodes.insert(ino, node.clone());
                        directory_listing.insert(
                            node.get_name().into(),
                            (
                                ino,
                                match node {
                                    Node::Directory(_) => fuser::FileType::Directory,
                                    Node::File(_) => fuser::FileType::RegularFile,
                                    Node::Symlink(_) => fuser::FileType::Symlink,
                                },
                            ),
                        );
                        self.next_inode += 1;
                    }

                    // wrap the listing in an Rc.
                    let directory_listing = Rc::new(directory_listing);

                    // insert the listing itself.
                    self.directories
                        .insert(directory_digest.clone(), directory_listing.clone());
                    // return it
                    Ok(directory_listing)
                }
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
                &file_attr::gen_file_attr(&Node::Directory(DirectoryNode::default()), ino),
            );
            return;
        }

        match self.inodes.get(&ino) {
            None => reply.error(libc::ENOENT),
            Some(node) => {
                debug!(node = ?node, "found node");
                reply.attr(&Duration::MAX, &file_attr::gen_file_attr(node, ino));
            }
        }
    }

    #[tracing::instrument(skip_all, fields(rq.parent_inode = parent, rq.name = ?name))]
    fn lookup(
        &mut self,
        _req: &Request,
        parent: u64,
        name: &std::ffi::OsStr,
        reply: fuser::ReplyEntry,
    ) {
        debug!("lookup");

        if parent == fuser::FUSE_ROOT_ID {
            // parse the name into a [StorePath].
            let store_path = if let Some(name) = name.to_str() {
                match StorePath::from_string(name) {
                    Ok(store_path) => store_path,
                    Err(e) => {
                        debug!(e=?e, "unable to parse as store path");
                        reply_with_entry(reply, Ok(None));
                        return;
                    }
                }
            } else {
                debug!("{name:?} is no string");
                reply_with_entry(reply, Ok(None));
                return;
            };

            // TODO: cloning here is stupid.
            let (ino, node) = if let Some(ino) = self.store_paths.get(&store_path) {
                // If we already have that store path, lookup the node for it.
                (*ino, self.inodes.get(ino).unwrap().clone())
            } else {
                // else, look up in PathInfoService.
                match self.path_info_service.get(store_path.digest) {
                    Err(e) => {
                        reply_with_entry(reply, Err(e));
                        return;
                    }
                    Ok(None) => {
                        // the pathinfo doesn't exists, so the file doesn't exist.
                        reply_with_entry(reply, Ok(None));
                        return;
                    }
                    Ok(Some(path_info)) => {
                        let node = path_info.node.unwrap().node.unwrap();

                        // add to [self.inodes] and [self.store_paths].
                        let ino = self.next_inode;
                        debug_assert!(!self.inodes.contains_key(&ino));
                        self.inodes.insert(ino, node.clone());
                        debug_assert!(!self.store_paths.contains_key(&store_path));
                        self.store_paths.insert(store_path, ino);
                        self.next_inode += 1;

                        if let Node::Directory(ref directory_node) = node {
                            let directory_digest =
                                &B3Digest::from_vec(directory_node.digest.clone()).unwrap();

                            // populate self.directories here, readdir can't, as
                            // it doesn't know the parent inode.
                            if let Err(_e) =
                                self.get_directory_listing(ino, parent, directory_digest)
                            {
                                reply.error(libc::ENOSYS);
                                return;
                            }
                        }

                        (ino, node)
                    }
                }
            };

            reply_with_entry(reply, Ok(Some(&gen_file_attr(&node, ino))));
        } else {
            // This is the "lookup for "a" inside inode 42.
            // We lookup the parent directory listing, which must exist.
            // In there, we search for the name and return the attr.
            // In case the node is a directory, it calls get_directory_listing
            // to get the children populated.

            // lookup node for parent inode in inodes, must exist
            let parent_node = self.inodes.get(&parent).unwrap();

            match parent_node {
                Node::File(_) | Node::Symlink(_) => {
                    // if the parent inode was not a directory, this is invalid
                    reply.error(libc::ENOSYS);
                }
                Node::Directory(parent_directory_node) => {
                    // look up the directory listing from self.directories
                    let parent_directory_digest =
                        &B3Digest::from_vec(parent_directory_node.digest.clone()).unwrap();

                    let parent_directory_listing = self
                        .directories
                        .get(parent_directory_digest)
                        .unwrap()
                        .clone();

                    match parent_directory_listing.get(name) {
                        None => {
                            // the name doesn't exist
                            reply.error(libc::ENOENT);
                        }
                        Some((ino, file_type)) => {
                            // We found it in the listing, so we can lookup the node.
                            let node = self.inodes.get(ino).unwrap().clone();

                            // Return its attributes, but in case it's a directory, also make sure
                            // we populate self.directories so subsequent lookups and readdir can find it.
                            if *file_type == FileType::Directory {
                                if let Node::Directory(DirectoryNode {
                                    name: _,
                                    digest: directory_digest,
                                    size: _,
                                }) = &node
                                {
                                    let directory_digest =
                                        B3Digest::from_vec(directory_digest.clone()).unwrap();
                                    // populate directory listing
                                    self.get_directory_listing(*ino, parent, &directory_digest)
                                        .unwrap();
                                }
                            }

                            // done populating, reply with entry.
                            reply_with_entry(reply, Ok(Some(&gen_file_attr(&node, *ino))));
                        }
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
            // reply.error(libc::EPERM);
            return;
        }

        // lookup the node by inode. We rely on the previous lookup populating everything.
        let node = self.inodes.get(&ino).unwrap();

        match node {
            Node::File(_) | Node::Symlink(_) => {
                warn!(node=?node, "invalid readdir");
                reply.error(libc::ENOSYS);
            }
            Node::Directory(directory_node) => {
                let directory_digest = &B3Digest::from_vec(directory_node.digest.clone()).unwrap();

                let directory_listing = self.directories.get(directory_digest).unwrap();

                for (i, (name, (ino, file_type))) in
                    directory_listing.iter().skip(offset as usize).enumerate()
                {
                    // the second parameter will become the "offset" parameter on the next call.
                    let full = reply.add(*ino, offset + i as i64 + 1_i64, *file_type, name);
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
        let node = self.inodes.get(&ino).unwrap();

        // read is invalid on non-files.
        match node {
            Node::Directory(_) | Node::Symlink(_) => {
                reply.error(libc::ENOSYS);
            }
            Node::File(file_node) => {
                let blob_digest = &B3Digest::from_vec(file_node.digest.to_vec()).unwrap();

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
        let node = self.inodes.get(&ino).unwrap();

        match node {
            Node::Directory(_) | Node::File(_) => {
                reply.error(libc::ENOSYS);
            }
            Node::Symlink(symlink_node) => reply.data(symlink_node.target.as_bytes()),
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
