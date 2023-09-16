mod file_attr;
mod inode_tracker;
mod inodes;

#[cfg(test)]
mod tests;

use crate::{
    blobservice::{BlobReader, BlobService},
    directoryservice::DirectoryService,
    fuse::{
        file_attr::gen_file_attr,
        inodes::{DirectoryInodeData, InodeData},
    },
    pathinfoservice::PathInfoService,
    proto::{node::Node, NamedNode},
    B3Digest, Error,
};
use fuser::{FileAttr, ReplyAttr, Request};
use nix_compat::store_path::StorePath;
use parking_lot::{deadlock, RwLock};
use std::{collections::HashMap, time::Duration};
use std::{ffi::OsStr, io};
use std::{os::unix::ffi::OsStrExt, thread};
use std::{
    path::Path,
    sync::{atomic::Ordering, Arc},
};
use std::{str::FromStr, sync::atomic::AtomicU64};
use tokio::io::{AsyncBufReadExt, AsyncSeekExt};
use tracing::{debug, error, info, info_span, warn};

use self::inode_tracker::InodeTracker;

/// This implements a read-only FUSE filesystem for a tvix-store
/// with the passed [BlobService], [DirectoryService] and [PathInfoService].
///
/// We don't allow listing on the root mountpoint (inode 0).
/// In the future, this might be made configurable once a listing method is
/// added to [self.path_info_service], and then show all store paths in that
/// store.
///
/// Linux uses inodes in filesystems. When implementing FUSE, most calls are
/// *for* a given inode.
///
/// This means, we need to have a stable mapping of inode numbers to the
/// corresponding store nodes.
///
/// We internally delegate all inode allocation and state keeping to the
/// inode tracker, and store the currently "explored" store paths together with
/// root inode of the root.
///
/// There's some places where inodes are allocated / data inserted into
/// the inode tracker, if not allocated before already:
///  - Processing a `lookup` request, either in the mount root, or somewhere
///    deeper
///  - Processing a `readdir` request
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

    /// Whether to (try) listing elements in the root.
    list_root: bool,

    /// This maps a given StorePath to the inode we allocated for the root inode.
    store_paths: RwLock<HashMap<StorePath, u64>>,

    /// This keeps track of inodes and data alongside them.
    inode_tracker: RwLock<InodeTracker>,

    /// This holds all open file handles
    file_handles: RwLock<HashMap<u64, tokio::sync::Mutex<Box<dyn BlobReader>>>>,

    next_file_handle: AtomicU64,

    tokio_handle: tokio::runtime::Handle,
}

impl FUSE {
    pub fn new(
        blob_service: Arc<dyn BlobService>,
        directory_service: Arc<dyn DirectoryService>,
        path_info_service: Arc<dyn PathInfoService>,
        list_root: bool,
    ) -> Self {
        thread::spawn(move || loop {
            thread::sleep(Duration::from_secs(10));
            let deadlocks = deadlock::check_deadlock();
            if deadlocks.is_empty() {
                continue;
            }

            println!("{} deadlocks detected", deadlocks.len());
            for (i, threads) in deadlocks.iter().enumerate() {
                println!("Deadlock #{}", i);
                for t in threads {
                    println!("Thread Id {:#?}", t.thread_id());
                    println!("{:#?}", t.backtrace());
                }
            }
        });
        Self {
            blob_service,
            directory_service,
            path_info_service,

            list_root,

            store_paths: RwLock::new(HashMap::default()),
            inode_tracker: RwLock::new(Default::default()),

            file_handles: RwLock::new(Default::default()),
            next_file_handle: AtomicU64::new(1),
            tokio_handle: tokio::runtime::Handle::current(),
        }
    }

    /// This will turn a lookup request for [std::ffi::OsStr] in the root to
    /// a ino and [InodeData].
    /// It will peek in [self.store_paths], and then either look it up from
    /// [self.inode_tracker],
    /// or otherwise fetch from [self.path_info_service], and then insert into
    /// [self.inode_tracker].
    fn name_in_root_to_ino_and_data(
        &self,
        name: &std::ffi::OsStr,
    ) -> Result<Option<(u64, Arc<InodeData>)>, Error> {
        // parse the name into a [StorePath].
        let store_path = if let Some(name) = name.to_str() {
            match StorePath::from_str(name) {
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

        let mut store_paths = self.store_paths.write();
        if let Some(ino) = store_paths.get(&store_path) {
            // If we already have that store path, lookup the inode from
            // self.store_paths and then get the data from [self.inode_tracker],
            // which in the case of a [InodeData::Directory] will be fully
            // populated.
            Ok(Some((
                *ino,
                self.inode_tracker.read().get(*ino).expect("must exist"),
            )))
        } else {
            // If we don't have it, look it up in PathInfoService.
            match self.path_info_service.get(store_path.digest)? {
                // the pathinfo doesn't exist, so the file doesn't exist.
                None => Ok(None),
                Some(path_info) => {
                    // The pathinfo does exist, so there must be a root node
                    let root_node = path_info.node.unwrap().node.unwrap();

                    // The name must match what's passed in the lookup, otherwise we return nothing.
                    if root_node.get_name() != store_path.to_string().as_bytes() {
                        return Ok(None);
                    }

                    // insert the (sparse) inode data and register in
                    // self.store_paths.
                    // FUTUREWORK: change put to return the data after
                    // inserting, so we don't need to lookup a second
                    // time?
                    let (ino, inode) = {
                        let mut inode_tracker = self.inode_tracker.write();
                        let ino = inode_tracker.put((&root_node).into());
                        (ino, inode_tracker.get(ino).unwrap())
                    };
                    store_paths.insert(store_path, ino);

                    Ok(Some((ino, inode)))
                }
            }
        }
    }

    /// This will lookup a directory by digest, and will turn it into a
    /// [InodeData::Directory(DirectoryInodeData::Populated(..))].
    /// This is both used to initially insert the root node of a store path,
    /// as well as when looking up an intermediate DirectoryNode.
    fn fetch_directory_inode_data(&self, directory_digest: &B3Digest) -> Result<InodeData, Error> {
        match self.directory_service.get(directory_digest) {
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
}

impl fuser::Filesystem for FUSE {
    #[tracing::instrument(skip_all, fields(rq.inode = ino))]
    fn getattr(&mut self, _req: &Request, ino: u64, reply: ReplyAttr) {
        debug!("getattr");

        if ino == fuser::FUSE_ROOT_ID {
            reply.attr(&Duration::MAX, &file_attr::ROOT_FILE_ATTR);
            return;
        }

        match self.inode_tracker.read().get(ino) {
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
                Err(e) => {
                    warn!("{}", e);
                    reply.error(libc::EIO);
                }
                Ok(None) => {
                    reply.error(libc::ENOENT);
                }
                Ok(Some((ino, inode_data))) => {
                    debug!(inode_data=?&inode_data, ino=ino, "Some");
                    reply_with_entry(reply, &gen_file_attr(&inode_data, ino));
                }
            }
        } else {
            // This is the "lookup for "a" inside inode 42.
            // We already know that inode 42 must be a directory.
            // It might not be populated yet, so if it isn't, we do (by
            // fetching from [self.directory_service]), and save the result in
            // [self.inode_tracker].
            // Now it for sure is populated, so we search for that name in the
            // list of children and return the FileAttrs.

            let mut inode_tracker = self.inode_tracker.write();
            let parent_data = inode_tracker.get(parent_ino).unwrap();
            let parent_data = match *parent_data {
                InodeData::Regular(..) | InodeData::Symlink(_) => {
                    // if the parent inode was not a directory, this doesn't make sense
                    reply.error(libc::ENOTDIR);
                    return;
                }
                InodeData::Directory(DirectoryInodeData::Sparse(ref parent_digest, _)) => {
                    match self.fetch_directory_inode_data(parent_digest) {
                        Ok(new_data) => {
                            // update data in [self.inode_tracker] with populated variant.
                            // FUTUREWORK: change put to return the data after
                            // inserting, so we don't need to lookup a second
                            // time?
                            let ino = inode_tracker.put(new_data);
                            inode_tracker.get(ino).unwrap()
                        }
                        Err(_e) => {
                            reply.error(libc::EIO);
                            return;
                        }
                    }
                }
                InodeData::Directory(DirectoryInodeData::Populated(..)) => parent_data,
            };

            // now parent_data can only be a [InodeData::Directory(DirectoryInodeData::Populated(..))].
            let (parent_digest, children) = if let InodeData::Directory(
                DirectoryInodeData::Populated(ref parent_digest, ref children),
            ) = *parent_data
            {
                (parent_digest, children)
            } else {
                panic!("unexpected type")
            };
            let span = info_span!("lookup", directory.digest = %parent_digest);
            let _enter = span.enter();

            // in the children, find the one with the desired name.
            if let Some((child_ino, _)) =
                children.iter().find(|e| e.1.get_name() == name.as_bytes())
            {
                // lookup the child [InodeData] in [self.inode_tracker].
                // We know the inodes for children have already been allocated.
                let child_inode_data = inode_tracker.get(*child_ino).unwrap();

                // Reply with the file attributes for the child.
                // For child directories, we still have all data we need to reply.
                reply_with_entry(reply, &gen_file_attr(&child_inode_data, *child_ino));
            } else {
                // Child not found, return ENOENT.
                reply.error(libc::ENOENT);
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
            if !self.list_root {
                reply.error(libc::EPERM); // same error code as ipfs/kubo
                return;
            } else {
                for (i, path_info) in self
                    .path_info_service
                    .list()
                    .skip(offset as usize)
                    .enumerate()
                {
                    let path_info = match path_info {
                        Err(e) => {
                            warn!("failed to retrieve pathinfo: {}", e);
                            reply.error(libc::EPERM);
                            return;
                        }
                        Ok(path_info) => path_info,
                    };

                    // We know the root node exists and the store_path can be parsed because clients MUST validate.
                    let root_node = path_info.node.unwrap().node.unwrap();
                    let store_path = StorePath::from_bytes(root_node.get_name()).unwrap();

                    let mut store_paths = self.store_paths.write();
                    let ino = match store_paths.get(&store_path) {
                        Some(ino) => *ino,
                        None => {
                            // insert the (sparse) inode data and register in
                            // self.store_paths.
                            let ino = self.inode_tracker.write().put((&root_node).into());
                            store_paths.insert(store_path.clone(), ino);
                            ino
                        }
                    };

                    let ty = match root_node {
                        Node::Directory(_) => fuser::FileType::Directory,
                        Node::File(_) => fuser::FileType::RegularFile,
                        Node::Symlink(_) => fuser::FileType::Symlink,
                    };

                    let full =
                        reply.add(ino, offset + i as i64 + 1_i64, ty, store_path.to_string());
                    if full {
                        break;
                    }
                }
                reply.ok();
                return;
            }
        }

        // lookup the inode data.
        let mut inode_tracker = self.inode_tracker.write();
        let dir_inode_data = inode_tracker.get(ino).unwrap();
        let dir_inode_data = match *dir_inode_data {
            InodeData::Regular(..) | InodeData::Symlink(..) => {
                warn!("Not a directory");
                reply.error(libc::ENOTDIR);
                return;
            }
            InodeData::Directory(DirectoryInodeData::Sparse(ref directory_digest, _)) => {
                match self.fetch_directory_inode_data(directory_digest) {
                    Ok(new_data) => {
                        // update data in [self.inode_tracker] with populated variant.
                        // FUTUREWORK: change put to return the data after
                        // inserting, so we don't need to lookup a second
                        // time?
                        let ino = inode_tracker.put(new_data.clone());
                        inode_tracker.get(ino).unwrap()
                    }
                    Err(_e) => {
                        reply.error(libc::EIO);
                        return;
                    }
                }
            }
            InodeData::Directory(DirectoryInodeData::Populated(..)) => dir_inode_data,
        };

        // now parent_data can only be InodeData::Directory(DirectoryInodeData::Populated(..))
        if let InodeData::Directory(DirectoryInodeData::Populated(ref _digest, ref children)) =
            *dir_inode_data
        {
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
                    std::ffi::OsStr::from_bytes(child_node.get_name()),
                );
                if full {
                    break;
                }
            }
            reply.ok();
        } else {
            panic!("unexpected type")
        }
    }

    #[tracing::instrument(skip_all, fields(rq.inode = ino))]
    fn open(&mut self, _req: &Request<'_>, ino: u64, _flags: i32, reply: fuser::ReplyOpen) {
        if ino == fuser::FUSE_ROOT_ID {
            reply.error(libc::ENOSYS);
            return;
        }

        // lookup the inode
        match *self.inode_tracker.read().get(ino).unwrap() {
            // read is invalid on non-files.
            InodeData::Directory(..) | InodeData::Symlink(_) => {
                warn!("is directory");
                reply.error(libc::EISDIR);
            }
            InodeData::Regular(ref blob_digest, _blob_size, _) => {
                let span = info_span!("read", blob.digest = %blob_digest);
                let _enter = span.enter();

                let blob_service = self.blob_service.clone();
                let blob_digest = blob_digest.clone();

                let task = self
                    .tokio_handle
                    .spawn(async move { blob_service.open_read(&blob_digest).await });

                let blob_reader = self.tokio_handle.block_on(task).unwrap();

                match blob_reader {
                    Ok(None) => {
                        warn!("blob not found");
                        reply.error(libc::EIO);
                    }
                    Err(e) => {
                        warn!(e=?e, "error opening blob");
                        reply.error(libc::EIO);
                    }
                    Ok(Some(blob_reader)) => {
                        // get a new file handle
                        // TODO: this will overflow after 2**64 operations,
                        // which is fine for now.
                        // See https://cl.tvl.fyi/c/depot/+/8834/comment/a6684ce0_d72469d1
                        // for the discussion on alternatives.
                        let fh = self.next_file_handle.fetch_add(1, Ordering::SeqCst);

                        debug!("add file handle {}", fh);
                        self.file_handles
                            .write()
                            .insert(fh, tokio::sync::Mutex::new(blob_reader));
                        reply.opened(fh, 0);
                    }
                }
            }
        }
    }

    #[tracing::instrument(skip_all, fields(rq.inode = ino, fh = fh))]
    fn release(
        &mut self,
        _req: &Request<'_>,
        ino: u64,
        fh: u64,
        _flags: i32,
        _lock_owner: Option<u64>,
        _flush: bool,
        reply: fuser::ReplyEmpty,
    ) {
        // remove and get ownership on the blob reader
        match self.file_handles.write().remove(&fh) {
            // drop it, which will close it.
            Some(blob_reader) => drop(blob_reader),
            None => {
                // These might already be dropped if a read error occured.
                debug!("file_handle {} not found", fh);
            }
        }

        reply.ok();
    }

    #[tracing::instrument(skip_all, fields(rq.inode = ino, rq.offset = offset, rq.size = size))]
    fn read(
        &mut self,
        _req: &Request<'_>,
        ino: u64,
        fh: u64,
        offset: i64,
        size: u32,
        _flags: i32,
        _lock_owner: Option<u64>,
        reply: fuser::ReplyData,
    ) {
        debug!("read");

        // We need to take out the blob reader from self.file_handles, so we can
        // interact with it in the separate task.
        // On success, we pass it back out of the task, so we can put it back in self.file_handles.
        let blob_reader = match self.file_handles.write().remove(&fh) {
            Some(blob_reader) => blob_reader,
            None => {
                warn!("file handle {} unknown", fh);
                reply.error(libc::EIO);
                return;
            }
        };

        let task = self.tokio_handle.spawn(async move {
            let mut blob_reader_guard = blob_reader.lock().await;
            // seek to the offset specified, which is relative to the start of the file.
            let resp = blob_reader_guard
                .seek(io::SeekFrom::Start(offset as u64))
                .await;

            match resp {
                Ok(pos) => {
                    debug_assert_eq!(offset as u64, pos);
                }
                Err(e) => {
                    warn!("failed to seek to offset {}: {}", offset, e);
                    return Err(libc::EIO);
                }
            }

            // As written in the fuser docs, read should send exactly the number
            // of bytes requested except on EOF or error.

            let mut buf: Vec<u8> = Vec::with_capacity(size as usize);

            while (buf.len() as u64) < size as u64 {
                match blob_reader_guard.fill_buf().await {
                    Ok(int_buf) => {
                        // copy things from the internal buffer into buf to fill it till up until size

                        // an empty buffer signals we reached EOF.
                        if int_buf.is_empty() {
                            break;
                        }

                        // calculate how many bytes we can read from int_buf.
                        // It's either all of int_buf, or the number of bytes missing in buf to reach size.
                        let len_to_copy = std::cmp::min(int_buf.len(), size as usize - buf.len());

                        // copy these bytes into our buffer
                        buf.extend_from_slice(&int_buf[..len_to_copy]);
                        // and consume them in the buffered reader.
                        blob_reader_guard.consume(len_to_copy);
                    }
                    Err(e) => return Err(e.raw_os_error().unwrap()),
                }
            }
            drop(blob_reader_guard);
            Ok((buf, blob_reader))
        });

        let resp = self.tokio_handle.block_on(task).unwrap();

        match resp {
            Err(e) => reply.error(e),
            Ok((buf, blob_reader)) => {
                reply.data(&buf);
                self.file_handles.write().insert(fh, blob_reader);
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
        match *self.inode_tracker.read().get(ino).unwrap() {
            InodeData::Directory(..) | InodeData::Regular(..) => {
                reply.error(libc::EINVAL);
            }
            InodeData::Symlink(ref target) => reply.data(target),
        }
    }
}

fn reply_with_entry(reply: fuser::ReplyEntry, file_attr: &FileAttr) {
    reply.entry(&Duration::MAX, file_attr, 1 /* TODO: generation */);
}

impl fuse_backend_rs::api::filesystem::FileSystem for FUSE {
    type Inode = u64;
    type Handle = u64;

    fn init(
        &self,
        _capable: fuse_backend_rs::api::filesystem::FsOptions,
    ) -> io::Result<fuse_backend_rs::api::filesystem::FsOptions> {
        use fuse_backend_rs::api::filesystem::FsOptions;
        Ok(FsOptions::empty())
    }

    #[tracing::instrument(skip_all, fields(rq.inode = inode))]
    fn getattr(
        &self,
        _ctx: &fuse_backend_rs::api::filesystem::Context,
        inode: Self::Inode,
        _handle: Option<Self::Handle>,
    ) -> io::Result<(libc::stat64, Duration)> {
        if inode == fuse_backend_rs::api::filesystem::ROOT_ID {
            return Ok((file_attr::ROOT_FILE_ATTR_LIBC, Duration::MAX));
        }

        match self.inode_tracker.read().get(inode) {
            None => return Err(io::Error::from_raw_os_error(libc::ENOENT)),
            Some(node) => {
                debug!(node = ?node, "found node");
                Ok((file_attr::gen_file_attr_libc(&node, inode), Duration::MAX))
            }
        }
    }

    #[tracing::instrument(skip_all, fields(rq.parent_inode = parent, rq.name = ?name))]
    fn lookup(
        &self,
        _ctx: &fuse_backend_rs::api::filesystem::Context,
        parent: Self::Inode,
        name: &std::ffi::CStr,
    ) -> io::Result<fuse_backend_rs::api::filesystem::Entry> {
        debug!("lookup");

        // TODO: Switch internal tracking to OsStr.
        let name = OsStr::from_bytes(name.to_bytes());

        // This goes from a parent inode to a node.
        // - If the parent is [fuser::FUSE_ROOT_ID], we need to check
        //   [self.store_paths] (fetching from PathInfoService if needed)
        // - Otherwise, lookup the parent in [self.inode_tracker] (which must be
        //   a [InodeData::Directory]), and find the child with that name.
        if parent == fuse_backend_rs::api::filesystem::ROOT_ID {
            return match self.name_in_root_to_ino_and_data(name) {
                Err(e) => {
                    warn!("{}", e);
                    Err(io::Error::from_raw_os_error(libc::ENOENT))
                }
                Ok(None) => Err(io::Error::from_raw_os_error(libc::ENOENT)),
                Ok(Some((ino, inode_data))) => {
                    debug!(inode_data=?&inode_data, ino=ino, "Some");
                    Ok(fuse_backend_rs::api::filesystem::Entry {
                        inode: ino,
                        attr: file_attr::gen_file_attr_libc(&inode_data, ino),
                        attr_timeout: Duration::MAX,
                        entry_timeout: Duration::MAX,
                        ..Default::default()
                    })
                }
            };
        }

        // This is the "lookup for "a" inside inode 42.
        // We already know that inode 42 must be a directory.
        // It might not be populated yet, so if it isn't, we do (by
        // fetching from [self.directory_service]), and save the result in
        // [self.inode_tracker].
        // Now it for sure is populated, so we search for that name in the
        // list of children and return the FileAttrs.

        let mut inode_tracker = self.inode_tracker.write();
        let parent_data = inode_tracker.get(parent).unwrap();
        let parent_data = match *parent_data {
            InodeData::Regular(..) | InodeData::Symlink(_) => {
                // if the parent inode was not a directory, this doesn't make sense
                return Err(io::Error::from_raw_os_error(libc::ENOTDIR));
            }
            InodeData::Directory(DirectoryInodeData::Sparse(ref parent_digest, _)) => {
                match self.fetch_directory_inode_data(parent_digest) {
                    Ok(new_data) => {
                        // update data in [self.inode_tracker] with populated variant.
                        // FUTUREWORK: change put to return the data after
                        // inserting, so we don't need to lookup a second
                        // time?
                        let ino = inode_tracker.put(new_data);
                        inode_tracker.get(ino).unwrap()
                    }
                    Err(_e) => {
                        return Err(io::Error::from_raw_os_error(libc::EIO));
                    }
                }
            }
            InodeData::Directory(DirectoryInodeData::Populated(..)) => parent_data,
        };

        // now parent_data can only be a [InodeData::Directory(DirectoryInodeData::Populated(..))].
        let (parent_digest, children) = if let InodeData::Directory(
            DirectoryInodeData::Populated(ref parent_digest, ref children),
        ) = *parent_data
        {
            (parent_digest, children)
        } else {
            panic!("unexpected type")
        };
        let span = info_span!("lookup", directory.digest = %parent_digest);
        let _enter = span.enter();

        // in the children, find the one with the desired name.
        if let Some((child_ino, _)) = children.iter().find(|e| e.1.get_name() == name.as_bytes()) {
            // lookup the child [InodeData] in [self.inode_tracker].
            // We know the inodes for children have already been allocated.
            let child_inode_data = inode_tracker.get(*child_ino).unwrap();

            // Reply with the file attributes for the child.
            // For child directories, we still have all data we need to reply.
            Ok(fuse_backend_rs::api::filesystem::Entry {
                inode: *child_ino,
                attr: file_attr::gen_file_attr_libc(&child_inode_data, *child_ino),
                attr_timeout: Duration::MAX,
                entry_timeout: Duration::MAX,
                ..Default::default()
            })
        } else {
            // Child not found, return ENOENT.
            Err(io::Error::from_raw_os_error(libc::ENOENT))
        }
    }

    // TODO: readdirplus?

    #[tracing::instrument(skip_all, fields(rq.inode = inode, rq.offset = offset))]
    fn readdir(
        &self,
        _ctx: &fuse_backend_rs::api::filesystem::Context,
        inode: Self::Inode,
        _handle: Self::Handle,
        _size: u32,
        offset: u64,
        add_entry: &mut dyn FnMut(fuse_backend_rs::api::filesystem::DirEntry) -> io::Result<usize>,
    ) -> io::Result<()> {
        debug!("readdir");

        if inode == fuse_backend_rs::api::filesystem::ROOT_ID {
            if !self.list_root {
                return Err(io::Error::from_raw_os_error(libc::EPERM)); // same error code as ipfs/kubo
            } else {
                for (i, path_info) in self
                    .path_info_service
                    .list()
                    .skip(offset as usize)
                    .enumerate()
                {
                    let path_info = match path_info {
                        Err(e) => {
                            warn!("failed to retrieve pathinfo: {}", e);
                            return Err(io::Error::from_raw_os_error(libc::EPERM));
                        }
                        Ok(path_info) => path_info,
                    };

                    // We know the root node exists and the store_path can be parsed because clients MUST validate.
                    let root_node = path_info.node.unwrap().node.unwrap();
                    let store_path = StorePath::from_bytes(root_node.get_name()).unwrap();

                    let mut store_paths = self.store_paths.write();
                    let ino = match store_paths.get(&store_path) {
                        Some(ino) => *ino,
                        None => {
                            // insert the (sparse) inode data and register in
                            // self.store_paths.
                            let ino = self.inode_tracker.write().put((&root_node).into());
                            store_paths.insert(store_path.clone(), ino);
                            ino
                        }
                    };

                    let ty = match root_node {
                        Node::Directory(_) => libc::S_IFDIR,
                        Node::File(_) => libc::S_IFREG,
                        Node::Symlink(_) => libc::S_IFLNK,
                    };

                    let written = add_entry(fuse_backend_rs::api::filesystem::DirEntry {
                        ino,
                        offset: offset + i as u64,
                        type_: ty,
                        name: store_path.to_string().as_bytes(),
                    })?;
                    // If the buffer is full, add_entry will return `Ok(0)`.
                    if written == 0 {
                        break;
                    }
                }
                return Ok(());
            }
        }

        // lookup the inode data.
        let mut inode_tracker = self.inode_tracker.write();
        let dir_inode_data = inode_tracker.get(inode).unwrap();
        let dir_inode_data = match *dir_inode_data {
            InodeData::Regular(..) | InodeData::Symlink(..) => {
                warn!("Not a directory");
                return Err(io::Error::from_raw_os_error(libc::ENOTDIR));
            }
            InodeData::Directory(DirectoryInodeData::Sparse(ref directory_digest, _)) => {
                match self.fetch_directory_inode_data(directory_digest) {
                    Ok(new_data) => {
                        // update data in [self.inode_tracker] with populated variant.
                        // FUTUREWORK: change put to return the data after
                        // inserting, so we don't need to lookup a second
                        // time?
                        let ino = inode_tracker.put(new_data.clone());
                        inode_tracker.get(ino).unwrap()
                    }
                    Err(_e) => {
                        return Err(io::Error::from_raw_os_error(libc::EIO));
                    }
                }
            }
            InodeData::Directory(DirectoryInodeData::Populated(..)) => dir_inode_data,
        };

        // now parent_data can only be InodeData::Directory(DirectoryInodeData::Populated(..))
        if let InodeData::Directory(DirectoryInodeData::Populated(ref _digest, ref children)) =
            *dir_inode_data
        {
            for (i, (ino, child_node)) in children.iter().skip(offset as usize).enumerate() {
                // the second parameter will become the "offset" parameter on the next call.
                let written = add_entry(fuse_backend_rs::api::filesystem::DirEntry {
                    ino: *ino,
                    offset: offset + i as u64 + 1,
                    type_: match child_node {
                        Node::Directory(_) => libc::S_IFDIR,
                        Node::File(_) => libc::S_IFREG,
                        Node::Symlink(_) => libc::S_IFLNK,
                    },
                    name: child_node.get_name(),
                })?;
                // If the buffer is full, add_entry will return `Ok(0)`.
                if written == 0 {
                    break;
                }
            }
        } else {
            panic!("unexpected type")
        }

        Ok(())
    }

    #[tracing::instrument(skip_all, fields(rq.inode = inode))]
    fn open(
        &self,
        _ctx: &fuse_backend_rs::api::filesystem::Context,
        inode: Self::Inode,
        _flags: u32,
        _fuse_flags: u32,
    ) -> io::Result<(
        Option<Self::Handle>,
        fuse_backend_rs::api::filesystem::OpenOptions,
    )> {
        if inode == fuse_backend_rs::api::filesystem::ROOT_ID {
            return Err(io::Error::from_raw_os_error(libc::ENOSYS));
        }

        // lookup the inode
        match *self.inode_tracker.read().get(inode).unwrap() {
            // read is invalid on non-files.
            InodeData::Directory(..) | InodeData::Symlink(_) => {
                warn!("is directory");
                return Err(io::Error::from_raw_os_error(libc::EISDIR));
            }
            InodeData::Regular(ref blob_digest, _blob_size, _) => {
                let span = info_span!("read", blob.digest = %blob_digest);
                let _enter = span.enter();

                let blob_service = self.blob_service.clone();
                let blob_digest = blob_digest.clone();

                let task = self
                    .tokio_handle
                    .spawn(async move { blob_service.open_read(&blob_digest).await });

                let blob_reader = self.tokio_handle.block_on(task).unwrap();

                match blob_reader {
                    Ok(None) => {
                        warn!("blob not found");
                        return Err(io::Error::from_raw_os_error(libc::EIO));
                    }
                    Err(e) => {
                        warn!(e=?e, "error opening blob");
                        return Err(io::Error::from_raw_os_error(libc::EIO));
                    }
                    Ok(Some(blob_reader)) => {
                        // get a new file handle
                        // TODO: this will overflow after 2**64 operations,
                        // which is fine for now.
                        // See https://cl.tvl.fyi/c/depot/+/8834/comment/a6684ce0_d72469d1
                        // for the discussion on alternatives.
                        let fh = self.next_file_handle.fetch_add(1, Ordering::SeqCst);

                        debug!("add file handle {}", fh);
                        self.file_handles
                            .write()
                            .insert(fh, tokio::sync::Mutex::new(blob_reader));

                        Ok((
                            Some(fh),
                            fuse_backend_rs::api::filesystem::OpenOptions::empty(),
                        ))
                    }
                }
            }
        }
    }

    #[tracing::instrument(skip_all, fields(rq.inode = inode, fh = handle))]
    fn release(
        &self,
        _ctx: &fuse_backend_rs::api::filesystem::Context,
        inode: Self::Inode,
        _flags: u32,
        handle: Self::Handle,
        _flush: bool,
        _flock_release: bool,
        _lock_owner: Option<u64>,
    ) -> io::Result<()> {
        // remove and get ownership on the blob reader
        match self.file_handles.write().remove(&handle) {
            // drop it, which will close it.
            Some(blob_reader) => drop(blob_reader),
            None => {
                // These might already be dropped if a read error occured.
                debug!("file_handle {} not found", handle);
            }
        }

        Ok(())
    }

    #[tracing::instrument(skip_all, fields(rq.inode = inode, rq.offset = offset, rq.size = size))]
    fn read(
        &self,
        _ctx: &fuse_backend_rs::api::filesystem::Context,
        inode: Self::Inode,
        handle: Self::Handle,
        w: &mut dyn fuse_backend_rs::api::filesystem::ZeroCopyWriter,
        size: u32,
        offset: u64,
        _lock_owner: Option<u64>,
        _flags: u32,
    ) -> io::Result<usize> {
        debug!("read");

        // TODO: Allow concurrent writes or at least finer-grained locking.
        let mut file_handles = self.file_handles.write();

        // We need to take out the blob reader from self.file_handles, so we can
        // interact with it in the separate task.
        // On success, we pass it back out of the task, so we can put it back in self.file_handles.
        let blob_reader = match file_handles.remove(&handle) {
            Some(blob_reader) => blob_reader,
            None => {
                warn!("file handle {} unknown", handle);
                return Err(io::Error::from_raw_os_error(libc::EIO));
            }
        };

        let task = self.tokio_handle.spawn(async move {
            let mut blob_reader_guard = blob_reader.lock().await;
            // seek to the offset specified, which is relative to the start of the file.
            let resp = blob_reader_guard
                .seek(io::SeekFrom::Start(offset as u64))
                .await;

            match resp {
                Ok(pos) => {
                    debug_assert_eq!(offset as u64, pos);
                }
                Err(e) => {
                    warn!("failed to seek to offset {}: {}", offset, e);
                    return Err(io::Error::from_raw_os_error(libc::EIO));
                }
            }

            // As written in the fuser docs, read should send exactly the number
            // of bytes requested except on EOF or error.

            let mut buf: Vec<u8> = Vec::with_capacity(size as usize);

            while (buf.len() as u64) < size as u64 {
                let int_buf = blob_reader_guard.fill_buf().await?;
                // copy things from the internal buffer into buf to fill it till up until size

                // an empty buffer signals we reached EOF.
                if int_buf.is_empty() {
                    break;
                }

                // calculate how many bytes we can read from int_buf.
                // It's either all of int_buf, or the number of bytes missing in buf to reach size.
                let len_to_copy = std::cmp::min(int_buf.len(), size as usize - buf.len());

                // copy these bytes into our buffer
                buf.extend_from_slice(&int_buf[..len_to_copy]);
                // and consume them in the buffered reader.
                blob_reader_guard.consume(len_to_copy);
            }
            drop(blob_reader_guard);
            Ok((buf, blob_reader))
        });

        let (buf, blob_reader) = self.tokio_handle.block_on(task).unwrap()?;

        file_handles.insert(handle, blob_reader);
        w.write(&buf)
    }

    #[tracing::instrument(skip_all, fields(rq.inode = inode))]
    fn readlink(
        &self,
        _ctx: &fuse_backend_rs::api::filesystem::Context,
        inode: Self::Inode,
    ) -> io::Result<Vec<u8>> {
        if inode == fuse_backend_rs::api::filesystem::ROOT_ID {
            return Err(io::Error::from_raw_os_error(libc::ENOSYS));
        }

        // lookup the inode
        match *self.inode_tracker.read().get(inode).unwrap() {
            InodeData::Directory(..) | InodeData::Regular(..) => {
                Err(io::Error::from_raw_os_error(libc::EINVAL))
            }
            InodeData::Symlink(ref target) => Ok(target.to_vec()),
        }
    }
}

use fuse_backend_rs::api::server::Server;
use fuse_backend_rs::transport::{FuseChannel, FuseSession};

/// A fusedev daemon example
#[allow(dead_code)]
pub struct Daemon {
    mountpoint: String,
    server: Arc<Server<Arc<FUSE>>>,
    thread_cnt: u32,
    session: Option<FuseSession>,
}

#[allow(dead_code)]
impl Daemon {
    /// Creates a fusedev daemon instance
    pub fn new(fuse: FUSE, mountpoint: &str, thread_cnt: u32) -> Self {
        Daemon {
            mountpoint: mountpoint.to_string(),
            server: Arc::new(Server::new(Arc::new(fuse))),
            thread_cnt,
            session: None,
        }
    }

    /// Mounts a fusedev daemon to the mountpoint, then start service threads to handle
    /// FUSE requests.
    pub fn mount(&mut self) -> io::Result<()> {
        let mut se =
            FuseSession::new(Path::new(&self.mountpoint), "tvix-store", "", false).unwrap();
        se.mount().unwrap();
        for _ in 0..self.thread_cnt {
            let mut server = FuseServer {
                server: self.server.clone(),
                ch: se.new_channel().unwrap(),
            };
            let _thread = thread::Builder::new()
                .name("fuse_server".to_string())
                .spawn(move || {
                    info!("new fuse thread");
                    let _ = server.svc_loop();
                    warn!("fuse service thread exits");
                })
                .unwrap();
        }
        self.session = Some(se);
        Ok(())
    }

    /// Umounts and destroys a fusedev daemon
    pub fn umount(&mut self) -> io::Result<()> {
        if let Some(mut se) = self.session.take() {
            se.umount().unwrap();
            se.wake().unwrap();
        }
        Ok(())
    }
}

impl Drop for Daemon {
    fn drop(&mut self) {
        let _ = self.umount();
    }
}

pub struct FuseServer {
    // TODO: Make a `new`
    pub server: Arc<Server<Arc<FUSE>>>,
    pub ch: FuseChannel,
}

impl FuseServer {
    pub fn svc_loop(&mut self) -> io::Result<()> {
        // Given error EBADF, it means kernel has shut down this session.
        let _ebadf = std::io::Error::from_raw_os_error(libc::EBADF);
        loop {
            if let Some((reader, writer)) = self
                .ch
                .get_request()
                .map_err(|_| std::io::Error::from_raw_os_error(libc::EINVAL))?
            {
                if let Err(e) = self
                    .server
                    .handle_message(reader, writer.into(), None, None)
                {
                    match e {
                        fuse_backend_rs::Error::EncodeMessage(_ebadf) => {
                            break;
                        }
                        _ => {
                            error!("Handling fuse message failed");
                            continue;
                        }
                    }
                }
            } else {
                info!("fuse server exits");
                break;
            }
        }
        Ok(())
    }
}
