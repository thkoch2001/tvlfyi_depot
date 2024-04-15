mod file_attr;
mod inode_tracker;
mod inodes;
mod root_nodes;

#[cfg(feature = "fuse")]
pub mod fuse;

#[cfg(feature = "virtiofs")]
pub mod virtiofs;

#[cfg(test)]
mod tests;

use crate::proto as castorepb;
use crate::{
    blobservice::{BlobReader, BlobService},
    directoryservice::DirectoryService,
    proto::{node::Node, NamedNode},
    B3Digest,
};
use bstr::ByteVec;
use fuse_backend_rs::abi::fuse_abi::{stat64, OpenOptions};
use fuse_backend_rs::api::filesystem::{
    Context, FileSystem, FsOptions, GetxattrReply, ListxattrReply, ROOT_ID,
};
use futures::StreamExt;
use parking_lot::RwLock;
use std::ffi::CStr;
use std::sync::Mutex;
use std::{
    collections::HashMap,
    io,
    sync::atomic::AtomicU64,
    sync::{atomic::Ordering, Arc},
    time::Duration,
};
use tokio::{
    io::{AsyncReadExt, AsyncSeekExt},
    sync::mpsc,
};
use tracing::{debug, info_span, instrument, warn};

pub use self::root_nodes::RootNodes;
use self::{
    file_attr::{gen_file_attr, ROOT_FILE_ATTR},
    inode_tracker::InodeTracker,
    inodes::{DirectoryInodeData, InodeData},
};

/// This implements a read-only FUSE filesystem for a tvix-store
/// with the passed [BlobService], [DirectoryService] and [RootNodes].
///
/// Linux uses inodes in filesystems. When implementing FUSE, most calls are
/// *for* a given inode.
///
/// This means, we need to have a stable mapping of inode numbers to the
/// corresponding store nodes.
///
/// We internally delegate all inode allocation and state keeping to the
/// inode tracker.
/// We store a mapping from currently "explored" names in the root to their
/// inode.
///
/// There's some places where inodes are allocated / data inserted into
/// the inode tracker, if not allocated before already:
///  - Processing a `lookup` request, either in the mount root, or somewhere
///    deeper.
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
/// allocation", aka reserve Directory.size inodes for each directory node we
/// explore.
/// Tests for this live in the tvix-store crate.
pub struct TvixStoreFs<BS, DS, RN> {
    blob_service: BS,
    directory_service: DS,
    root_nodes_provider: RN,

    /// Whether to (try) listing elements in the root.
    list_root: bool,

    /// Whether to expose blob and directory digests as extended attributes.
    show_xattr: bool,

    /// This maps a given basename in the root to the inode we allocated for the node.
    root_nodes: RwLock<HashMap<Vec<u8>, u64>>,

    /// This keeps track of inodes and data alongside them.
    inode_tracker: RwLock<InodeTracker>,

    /// Maps from the handle returned from an opendir to
    /// This holds all opendir handles (for the root inode)
    /// They point to the rx part of the channel producing the listing.
    #[allow(clippy::type_complexity)]
    dir_handles:
        RwLock<HashMap<u64, Arc<Mutex<mpsc::Receiver<(usize, Result<Node, crate::Error>)>>>>>,

    next_dir_handle: AtomicU64,

    /// This holds all open file handles
    #[allow(clippy::type_complexity)]
    file_handles: RwLock<HashMap<u64, Arc<Mutex<Box<dyn BlobReader>>>>>,

    next_file_handle: AtomicU64,

    tokio_handle: tokio::runtime::Handle,
}

impl<BS, DS, RN> TvixStoreFs<BS, DS, RN>
where
    BS: AsRef<dyn BlobService> + Clone + Send,
    DS: AsRef<dyn DirectoryService> + Clone + Send + 'static,
    RN: RootNodes + Clone + 'static,
{
    pub fn new(
        blob_service: BS,
        directory_service: DS,
        root_nodes_provider: RN,
        list_root: bool,
        show_xattr: bool,
    ) -> Self {
        Self {
            blob_service,
            directory_service,
            root_nodes_provider,

            list_root,
            show_xattr,

            root_nodes: RwLock::new(HashMap::default()),
            inode_tracker: RwLock::new(Default::default()),

            dir_handles: RwLock::new(Default::default()),
            next_dir_handle: AtomicU64::new(1),

            file_handles: RwLock::new(Default::default()),
            next_file_handle: AtomicU64::new(1),
            tokio_handle: tokio::runtime::Handle::current(),
        }
    }

    /// Retrieves the inode for a given root node basename, if present.
    /// This obtains a read lock on self.root_nodes.
    fn get_inode_for_root_name(&self, name: &[u8]) -> Option<u64> {
        self.root_nodes.read().get(name).cloned()
    }

    /// For a given inode, look up the given directory behind it (from
    /// self.inode_tracker), and return its children.
    /// The inode_tracker MUST know about this inode already, and it MUST point
    /// to a [InodeData::Directory].
    /// It is ok if it's a [DirectoryInodeData::Sparse] - in that case, a lookup
    /// in self.directory_service is performed, and self.inode_tracker is updated with the
    /// [DirectoryInodeData::Populated].
    #[instrument(skip(self), err)]
    fn get_directory_children(&self, ino: u64) -> io::Result<(B3Digest, Vec<(u64, Node)>)> {
        let data = self.inode_tracker.read().get(ino).unwrap();
        match *data {
            // if it's populated already, return children.
            InodeData::Directory(DirectoryInodeData::Populated(
                ref parent_digest,
                ref children,
            )) => Ok((parent_digest.clone(), children.clone())),
            // if it's sparse, fetch data using directory_service, populate child nodes
            // and update it in [self.inode_tracker].
            InodeData::Directory(DirectoryInodeData::Sparse(ref parent_digest, _)) => {
                let directory = self
                    .tokio_handle
                    .block_on({
                        let directory_service = self.directory_service.clone();
                        let parent_digest = parent_digest.to_owned();
                        async move { directory_service.as_ref().get(&parent_digest).await }
                    })?
                    .ok_or_else(|| {
                        warn!(directory.digest=%parent_digest, "directory not found");
                        // If the Directory can't be found, this is a hole, bail out.
                        io::Error::from_raw_os_error(libc::EIO)
                    })?;

                // Turn the retrieved directory into a InodeData::Directory(DirectoryInodeData::Populated(..)),
                // allocating inodes for the children on the way.
                let children = {
                    let mut inode_tracker = self.inode_tracker.write();

                    let children: Vec<(u64, castorepb::node::Node)> = directory
                        .nodes()
                        .map(|child_node| {
                            let child_ino = inode_tracker.put((&child_node).into());
                            (child_ino, child_node)
                        })
                        .collect();

                    // replace.
                    inode_tracker.replace(
                        ino,
                        Arc::new(InodeData::Directory(DirectoryInodeData::Populated(
                            parent_digest.clone(),
                            children.clone(),
                        ))),
                    );

                    children
                };

                Ok((parent_digest.clone(), children))
            }
            // if the parent inode was not a directory, this doesn't make sense
            InodeData::Regular(..) | InodeData::Symlink(_) => {
                Err(io::Error::from_raw_os_error(libc::ENOTDIR))
            }
        }
    }

    /// This will turn a lookup request for a name in the root to a ino and
    /// [InodeData].
    /// It will peek in [self.root_nodes], and then either look it up from
    /// [self.inode_tracker],
    /// or otherwise fetch from [self.root_nodes], and then insert into
    /// [self.inode_tracker].
    /// In the case the name can't be found, a libc::ENOENT is returned.
    fn name_in_root_to_ino_and_data(
        &self,
        name: &std::ffi::CStr,
    ) -> io::Result<(u64, Arc<InodeData>)> {
        // Look up the inode for that root node.
        // If there's one, [self.inode_tracker] MUST also contain the data,
        // which we can then return.
        if let Some(inode) = self.get_inode_for_root_name(name.to_bytes()) {
            return Ok((
                inode,
                self.inode_tracker
                    .read()
                    .get(inode)
                    .expect("must exist")
                    .to_owned(),
            ));
        }

        // We don't have it yet, look it up in [self.root_nodes].
        match self.tokio_handle.block_on({
            let root_nodes_provider = self.root_nodes_provider.clone();
            async move { root_nodes_provider.get_by_basename(name.to_bytes()).await }
        }) {
            // if there was an error looking up the root node, propagate up an IO error.
            Err(_e) => Err(io::Error::from_raw_os_error(libc::EIO)),
            // the root node doesn't exist, so the file doesn't exist.
            Ok(None) => Err(io::Error::from_raw_os_error(libc::ENOENT)),
            // The root node does exist
            Ok(Some(ref root_node)) => {
                // The name must match what's passed in the lookup, otherwise this is also a ENOENT.
                if root_node.get_name() != name.to_bytes() {
                    debug!(root_node.name=?root_node.get_name(), found_node.name=%name.to_string_lossy(), "node name mismatch");
                    return Err(io::Error::from_raw_os_error(libc::ENOENT));
                }

                // Let's check if someone else beat us to updating the inode tracker and
                // root_nodes map. This avoids locking inode_tracker for writing.
                if let Some(ino) = self.root_nodes.read().get(name.to_bytes()) {
                    return Ok((
                        *ino,
                        self.inode_tracker.read().get(*ino).expect("must exist"),
                    ));
                }

                // Only in case it doesn't, lock [self.root_nodes] and
                // [self.inode_tracker] for writing.
                let mut root_nodes = self.root_nodes.write();
                let mut inode_tracker = self.inode_tracker.write();

                // insert the (sparse) inode data and register in
                // self.root_nodes.
                let inode_data: InodeData = root_node.into();
                let ino = inode_tracker.put(inode_data.clone());
                root_nodes.insert(name.to_bytes().into(), ino);

                Ok((ino, Arc::new(inode_data)))
            }
        }
    }
}

const XATTR_NAME_DIRECTORY_DIGEST: &[u8] = b"user.tvix.castore.directory.digest";
const XATTR_NAME_BLOB_DIGEST: &[u8] = b"user.tvix.castore.blob.digest";

impl<BS, DS, RN> FileSystem for TvixStoreFs<BS, DS, RN>
where
    BS: AsRef<dyn BlobService> + Clone + Send + 'static,
    DS: AsRef<dyn DirectoryService> + Send + Clone + 'static,
    RN: RootNodes + Clone + 'static,
{
    type Handle = u64;
    type Inode = u64;

    fn init(&self, _capable: FsOptions) -> io::Result<FsOptions> {
        Ok(FsOptions::empty())
    }

    #[tracing::instrument(skip_all, fields(rq.inode = inode))]
    fn getattr(
        &self,
        _ctx: &Context,
        inode: Self::Inode,
        _handle: Option<Self::Handle>,
    ) -> io::Result<(stat64, Duration)> {
        if inode == ROOT_ID {
            return Ok((ROOT_FILE_ATTR.into(), Duration::MAX));
        }

        match self.inode_tracker.read().get(inode) {
            None => Err(io::Error::from_raw_os_error(libc::ENOENT)),
            Some(node) => {
                debug!(node = ?node, "found node");
                Ok((gen_file_attr(&node, inode).into(), Duration::MAX))
            }
        }
    }

    #[tracing::instrument(skip_all, fields(rq.parent_inode = parent, rq.name = ?name))]
    fn lookup(
        &self,
        _ctx: &Context,
        parent: Self::Inode,
        name: &std::ffi::CStr,
    ) -> io::Result<fuse_backend_rs::api::filesystem::Entry> {
        debug!("lookup");

        // This goes from a parent inode to a node.
        // - If the parent is [ROOT_ID], we need to check
        //   [self.root_nodes] (fetching from a [RootNode] provider if needed)
        // - Otherwise, lookup the parent in [self.inode_tracker] (which must be
        //   a [InodeData::Directory]), and find the child with that name.
        if parent == ROOT_ID {
            let (ino, inode_data) = self.name_in_root_to_ino_and_data(name)?;

            debug!(inode_data=?&inode_data, ino=ino, "Some");
            return Ok(fuse_backend_rs::api::filesystem::Entry {
                inode: ino,
                attr: gen_file_attr(&inode_data, ino).into(),
                attr_timeout: Duration::MAX,
                entry_timeout: Duration::MAX,
                ..Default::default()
            });
        }
        // This is the "lookup for "a" inside inode 42.
        // We already know that inode 42 must be a directory.
        let (parent_digest, children) = self.get_directory_children(parent)?;

        let span = info_span!("lookup", directory.digest = %parent_digest);
        let _enter = span.enter();

        // Search for that name in the list of children and return the FileAttrs.

        // in the children, find the one with the desired name.
        if let Some((child_ino, _)) = children.iter().find(|e| e.1.get_name() == name.to_bytes()) {
            // lookup the child [InodeData] in [self.inode_tracker].
            // We know the inodes for children have already been allocated.
            let child_inode_data = self.inode_tracker.read().get(*child_ino).unwrap();

            // Reply with the file attributes for the child.
            // For child directories, we still have all data we need to reply.
            Ok(fuse_backend_rs::api::filesystem::Entry {
                inode: *child_ino,
                attr: gen_file_attr(&child_inode_data, *child_ino).into(),
                attr_timeout: Duration::MAX,
                entry_timeout: Duration::MAX,
                ..Default::default()
            })
        } else {
            // Child not found, return ENOENT.
            Err(io::Error::from_raw_os_error(libc::ENOENT))
        }
    }

    #[tracing::instrument(skip_all, fields(rq.inode = inode))]
    fn opendir(
        &self,
        _ctx: &Context,
        inode: Self::Inode,
        _flags: u32,
    ) -> io::Result<(Option<Self::Handle>, OpenOptions)> {
        // In case opendir on the root is called, we provide the handle, as re-entering that listing is expensive.
        // For all other directory inodes we just let readdir take care of it.
        if inode == ROOT_ID {
            if !self.list_root {
                return Err(io::Error::from_raw_os_error(libc::EPERM)); // same error code as ipfs/kubo
            }

            let root_nodes_provider = self.root_nodes_provider.clone();
            let (tx, rx) = mpsc::channel(16);

            // This task will run in the background immediately and will exit
            // after the stream ends or if we no longer want any more entries.
            self.tokio_handle.spawn(async move {
                let mut stream = root_nodes_provider.list().enumerate();
                while let Some(node) = stream.next().await {
                    if tx.send(node).await.is_err() {
                        // If we get a send error, it means the sync code
                        // doesn't want any more entries.
                        break;
                    }
                }
            });

            // Put the rx part into [self.dir_handles].
            // TODO: this will overflow after 2**64 operations,
            // which is fine for now.
            // See https://cl.tvl.fyi/c/depot/+/8834/comment/a6684ce0_d72469d1
            // for the discussion on alternatives.
            let dh = self.next_dir_handle.fetch_add(1, Ordering::SeqCst);

            debug!("add dir handle {}", dh);
            self.dir_handles
                .write()
                .insert(dh, Arc::new(Mutex::new(rx)));

            return Ok((
                Some(dh),
                fuse_backend_rs::api::filesystem::OpenOptions::empty(), // TODO: non-seekable
            ));
        }

        Ok((None, OpenOptions::empty()))
    }

    #[tracing::instrument(skip_all, fields(rq.inode = inode, rq.handle = handle, rq.offset = offset))]
    fn readdir(
        &self,
        _ctx: &Context,
        inode: Self::Inode,
        handle: Self::Handle,
        _size: u32,
        offset: u64,
        add_entry: &mut dyn FnMut(fuse_backend_rs::api::filesystem::DirEntry) -> io::Result<usize>,
    ) -> io::Result<()> {
        debug!("readdir");

        if inode == ROOT_ID {
            if !self.list_root {
                return Err(io::Error::from_raw_os_error(libc::EPERM)); // same error code as ipfs/kubo
            }

            // get the handle from [self.dir_handles]
            let rx = match self.dir_handles.read().get(&handle) {
                Some(rx) => rx.clone(),
                None => {
                    warn!("dir handle {} unknown", handle);
                    return Err(io::Error::from_raw_os_error(libc::EIO));
                }
            };

            let mut rx = rx
                .lock()
                .map_err(|_| crate::Error::StorageError("mutex poisoned".into()))?;

            while let Some((i, n)) = rx.blocking_recv() {
                let root_node = n.map_err(|e| {
                    warn!("failed to retrieve root node: {}", e);
                    io::Error::from_raw_os_error(libc::EPERM)
                })?;

                let name = root_node.get_name();
                let ty = match root_node {
                    Node::Directory(_) => libc::S_IFDIR,
                    Node::File(_) => libc::S_IFREG,
                    Node::Symlink(_) => libc::S_IFLNK,
                };

                // obtain the inode, or allocate a new one.
                let ino = self.get_inode_for_root_name(name).unwrap_or_else(|| {
                    // insert the (sparse) inode data and register in
                    // self.root_nodes.
                    let ino = self.inode_tracker.write().put((&root_node).into());
                    self.root_nodes.write().insert(name.into(), ino);
                    ino
                });

                #[cfg(target_os = "macos")]
                let ty = ty as u32;

                let written = add_entry(fuse_backend_rs::api::filesystem::DirEntry {
                    ino,
                    offset: offset + i as u64 + 1,
                    type_: ty,
                    name,
                })?;
                // If the buffer is full, add_entry will return `Ok(0)`.
                if written == 0 {
                    break;
                }
            }
            return Ok(());
        }

        // Non root-node case: lookup the children, or return an error if it's not a directory.
        let (parent_digest, children) = self.get_directory_children(inode)?;

        let span = info_span!("lookup", directory.digest = %parent_digest);
        let _enter = span.enter();

        for (i, (ino, child_node)) in children.iter().skip(offset as usize).enumerate() {
            // the second parameter will become the "offset" parameter on the next call.
            let written = add_entry(fuse_backend_rs::api::filesystem::DirEntry {
                ino: *ino,
                offset: offset + i as u64 + 1,
                type_: match child_node {
                    #[allow(clippy::unnecessary_cast)]
                    // libc::S_IFDIR is u32 on Linux and u16 on MacOS
                    Node::Directory(_) => libc::S_IFDIR as u32,
                    #[allow(clippy::unnecessary_cast)]
                    // libc::S_IFDIR is u32 on Linux and u16 on MacOS
                    Node::File(_) => libc::S_IFREG as u32,
                    #[allow(clippy::unnecessary_cast)]
                    // libc::S_IFDIR is u32 on Linux and u16 on MacOS
                    Node::Symlink(_) => libc::S_IFLNK as u32,
                },
                name: child_node.get_name(),
            })?;
            // If the buffer is full, add_entry will return `Ok(0)`.
            if written == 0 {
                break;
            }
        }

        Ok(())
    }
    // TODO: readdirplus?

    #[tracing::instrument(skip_all, fields(rq.inode = inode, rq.handle = handle))]
    fn releasedir(
        &self,
        _ctx: &Context,
        inode: Self::Inode,
        _flags: u32,
        handle: Self::Handle,
    ) -> io::Result<()> {
        if inode == ROOT_ID {
            // drop the rx part of the channel.
            match self.dir_handles.write().remove(&handle) {
                // drop it, which will close it.
                Some(rx) => drop(rx),
                None => {
                    debug!("dir handle not found");
                }
            }
        }

        Ok(())
    }

    #[tracing::instrument(skip_all, fields(rq.inode = inode))]
    fn open(
        &self,
        _ctx: &Context,
        inode: Self::Inode,
        _flags: u32,
        _fuse_flags: u32,
    ) -> io::Result<(
        Option<Self::Handle>,
        fuse_backend_rs::api::filesystem::OpenOptions,
    )> {
        if inode == ROOT_ID {
            return Err(io::Error::from_raw_os_error(libc::ENOSYS));
        }

        // lookup the inode
        match *self.inode_tracker.read().get(inode).unwrap() {
            // read is invalid on non-files.
            InodeData::Directory(..) | InodeData::Symlink(_) => {
                warn!("is directory");
                Err(io::Error::from_raw_os_error(libc::EISDIR))
            }
            InodeData::Regular(ref blob_digest, _blob_size, _) => {
                let span = info_span!("read", blob.digest = %blob_digest);
                let _enter = span.enter();

                match self.tokio_handle.block_on({
                    let blob_service = self.blob_service.clone();
                    let blob_digest = blob_digest.clone();
                    async move { blob_service.as_ref().open_read(&blob_digest).await }
                }) {
                    Ok(None) => {
                        warn!("blob not found");
                        Err(io::Error::from_raw_os_error(libc::EIO))
                    }
                    Err(e) => {
                        warn!(e=?e, "error opening blob");
                        Err(io::Error::from_raw_os_error(libc::EIO))
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
                            .insert(fh, Arc::new(Mutex::new(blob_reader)));

                        Ok((
                            Some(fh),
                            fuse_backend_rs::api::filesystem::OpenOptions::empty(),
                        ))
                    }
                }
            }
        }
    }

    #[tracing::instrument(skip_all, fields(rq.inode = inode, rq.handle = handle))]
    fn release(
        &self,
        _ctx: &Context,
        inode: Self::Inode,
        _flags: u32,
        handle: Self::Handle,
        _flush: bool,
        _flock_release: bool,
        _lock_owner: Option<u64>,
    ) -> io::Result<()> {
        match self.file_handles.write().remove(&handle) {
            // drop the blob reader, which will close it.
            Some(blob_reader) => drop(blob_reader),
            None => {
                // These might already be dropped if a read error occured.
                debug!("file handle not found");
            }
        }

        Ok(())
    }

    #[tracing::instrument(skip_all, fields(rq.inode = inode, rq.offset = offset, rq.size = size))]
    fn read(
        &self,
        _ctx: &Context,
        inode: Self::Inode,
        handle: Self::Handle,
        w: &mut dyn fuse_backend_rs::api::filesystem::ZeroCopyWriter,
        size: u32,
        offset: u64,
        _lock_owner: Option<u64>,
        _flags: u32,
    ) -> io::Result<usize> {
        debug!("read");

        // We need to take out the blob reader from self.file_handles, so we can
        // interact with it in the separate task.
        // On success, we pass it back out of the task, so we can put it back in self.file_handles.
        let blob_reader = self
            .file_handles
            .read()
            .get(&handle)
            .ok_or_else(|| {
                warn!("file handle {} unknown", handle);
                io::Error::from_raw_os_error(libc::EIO)
            })
            .cloned()?;

        let mut blob_reader = blob_reader
            .lock()
            .map_err(|_| crate::Error::StorageError("mutex poisoned".into()))?;

        let buf = self.tokio_handle.block_on(async move {
            // seek to the offset specified, which is relative to the start of the file.
            let pos = blob_reader
                .seek(io::SeekFrom::Start(offset))
                .await
                .map_err(|e| {
                    warn!("failed to seek to offset {}: {}", offset, e);
                    io::Error::from_raw_os_error(libc::EIO)
                })?;

            debug_assert_eq!(offset, pos);

            // As written in the fuse docs, read should send exactly the number
            // of bytes requested except on EOF or error.

            let mut buf: Vec<u8> = Vec::with_capacity(size as usize);

            // copy things from the internal buffer into buf to fill it till up until size
            tokio::io::copy(&mut blob_reader.as_mut().take(size as u64), &mut buf).await?;

            Ok::<_, std::io::Error>(buf)
        })?;

        w.write(&buf)
    }

    #[tracing::instrument(skip_all, fields(rq.inode = inode))]
    fn readlink(&self, _ctx: &Context, inode: Self::Inode) -> io::Result<Vec<u8>> {
        if inode == ROOT_ID {
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

    #[tracing::instrument(skip_all, fields(rq.inode = inode, name=?name))]
    fn getxattr(
        &self,
        _ctx: &Context,
        inode: Self::Inode,
        name: &CStr,
        size: u32,
    ) -> io::Result<GetxattrReply> {
        if !self.show_xattr {
            return Err(io::Error::from_raw_os_error(libc::ENOSYS));
        }

        // Peek at the inode requested, and construct the response.
        let digest_str = match *self
            .inode_tracker
            .read()
            .get(inode)
            .ok_or_else(|| io::Error::from_raw_os_error(libc::ENODATA))?
        {
            InodeData::Directory(DirectoryInodeData::Sparse(ref digest, _))
            | InodeData::Directory(DirectoryInodeData::Populated(ref digest, _))
                if name.to_bytes() == XATTR_NAME_DIRECTORY_DIGEST =>
            {
                digest.to_string()
            }
            InodeData::Regular(ref digest, _, _) if name.to_bytes() == XATTR_NAME_BLOB_DIGEST => {
                digest.to_string()
            }
            _ => {
                return Err(io::Error::from_raw_os_error(libc::ENODATA));
            }
        };

        if size == 0 {
            Ok(GetxattrReply::Count(digest_str.len() as u32))
        } else if size < digest_str.len() as u32 {
            Err(io::Error::from_raw_os_error(libc::ERANGE))
        } else {
            Ok(GetxattrReply::Value(digest_str.into_bytes()))
        }
    }

    #[tracing::instrument(skip_all, fields(rq.inode = inode))]
    fn listxattr(
        &self,
        _ctx: &Context,
        inode: Self::Inode,
        size: u32,
    ) -> io::Result<ListxattrReply> {
        if !self.show_xattr {
            return Err(io::Error::from_raw_os_error(libc::ENOSYS));
        }

        // determine the (\0-terminated list) to of xattr keys present, depending on the type of the inode.
        let xattrs_names = {
            let mut out = Vec::new();
            if let Some(inode_data) = self.inode_tracker.read().get(inode) {
                match *inode_data {
                    InodeData::Directory(_) => {
                        out.extend_from_slice(XATTR_NAME_DIRECTORY_DIGEST);
                        out.push_byte(b'\x00');
                    }
                    InodeData::Regular(..) => {
                        out.extend_from_slice(XATTR_NAME_BLOB_DIGEST);
                        out.push_byte(b'\x00');
                    }
                    _ => {}
                }
            }
            out
        };

        if size == 0 {
            Ok(ListxattrReply::Count(xattrs_names.len() as u32))
        } else if size < xattrs_names.len() as u32 {
            Err(io::Error::from_raw_os_error(libc::ERANGE))
        } else {
            Ok(ListxattrReply::Names(xattrs_names.to_vec()))
        }
    }
}
