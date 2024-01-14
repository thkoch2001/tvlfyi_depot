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
use async_stream::try_stream;
use fuse_backend_rs::abi::fuse_abi::{stat64, CreateIn};
use fuse_backend_rs::api::filesystem::{
    AsyncFileSystem, AsyncZeroCopyReader, AsyncZeroCopyWriter, Context, Entry, FileSystem,
    FsOptions, OpenOptions, OwnedDirEntry, SetattrValid, ROOT_ID,
};
use futures::stream::BoxStream;
use futures::StreamExt;
use parking_lot::RwLock;
use std::ffi::CStr;
use std::{
    collections::HashMap,
    io,
    sync::atomic::AtomicU64,
    sync::{atomic::Ordering, Arc},
    time::Duration,
};
use tokio::io::{AsyncReadExt, AsyncSeekExt};
use tonic::async_trait;
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

    /// This maps a given basename in the root to the inode we allocated for the node.
    root_nodes: RwLock<HashMap<Vec<u8>, u64>>,

    /// This keeps track of inodes and data alongside them.
    inode_tracker: RwLock<InodeTracker>,

    /// This holds all open file handles
    #[allow(clippy::type_complexity)]
    file_handles: RwLock<HashMap<u64, Arc<tokio::sync::Mutex<Box<dyn BlobReader>>>>>,

    next_file_handle: AtomicU64,
}

impl<BS, DS, RN> TvixStoreFs<BS, DS, RN>
where
    BS: BlobService,
    DS: DirectoryService,
    RN: RootNodes,
{
    pub fn new(
        blob_service: BS,
        directory_service: DS,
        root_nodes_provider: RN,
        list_root: bool,
    ) -> Self {
        Self {
            blob_service,
            directory_service,
            root_nodes_provider,

            list_root,

            root_nodes: RwLock::new(HashMap::default()),
            inode_tracker: RwLock::new(Default::default()),

            file_handles: RwLock::new(Default::default()),
            next_file_handle: AtomicU64::new(1),
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
    async fn get_directory_children(&self, ino: u64) -> io::Result<(B3Digest, Vec<(u64, Node)>)> {
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
                    .directory_service
                    .get(parent_digest)
                    .await?
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
    async fn name_in_root_to_ino_and_data(
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
        match self
            .root_nodes_provider
            .get_by_basename(name.to_bytes())
            .await
        {
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

impl<BS, DS, RN> FileSystem for TvixStoreFs<BS, DS, RN>
where
    BS: BlobService,
    DS: DirectoryService,
    RN: RootNodes,
{
    type Handle = u64;
    type Inode = u64;

    fn init(&self, _capable: FsOptions) -> io::Result<FsOptions> {
        Ok(FsOptions::empty())
    }

    #[tracing::instrument(skip_all, fields(rq.inode = inode, fh = handle))]
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
}

#[async_trait(?Send)]
impl<BS, DS, RN> AsyncFileSystem for TvixStoreFs<BS, DS, RN>
where
    BS: BlobService,
    DS: DirectoryService,
    RN: RootNodes,
{
    #[tracing::instrument(skip_all, fields(rq.parent_inode = parent, rq.name = ?name))]
    async fn async_lookup(
        &self,
        _ctx: &Context,
        parent: Self::Inode,
        name: &CStr,
    ) -> io::Result<Entry> {
        debug!("lookup");

        // This goes from a parent inode to a node.
        // - If the parent is [ROOT_ID], we need to check
        //   [self.root_nodes] (fetching from a [RootNode] provider if needed)
        // - Otherwise, lookup the parent in [self.inode_tracker] (which must be
        //   a [InodeData::Directory]), and find the child with that name.
        if parent == ROOT_ID {
            let (ino, inode_data) = self.name_in_root_to_ino_and_data(name).await?;

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
        let (parent_digest, children) = self.get_directory_children(parent).await?;

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
    async fn async_getattr(
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

    #[tracing::instrument(skip_all, fields(rq.inode = inode))]
    async fn async_open(
        &self,
        _ctx: &Context,
        inode: Self::Inode,
        _flags: u32,
        _fuse_flags: u32,
    ) -> io::Result<(Option<Self::Handle>, OpenOptions)> {
        if inode == ROOT_ID {
            return Err(io::Error::from_raw_os_error(libc::ENOSYS));
        }

        // lookup the inode
        let inode_data = self.inode_tracker.read().get(inode).unwrap().clone();
        match *inode_data {
            // read is invalid on non-files.
            InodeData::Directory(..) | InodeData::Symlink(_) => {
                warn!("is directory");
                Err(io::Error::from_raw_os_error(libc::EISDIR))
            }
            InodeData::Regular(ref blob_digest, _blob_size, _) => {
                // TODO(cbrewster): Switch to instrument, span enter is not async compatible.
                let span = info_span!("read", blob.digest = %blob_digest);
                let _enter = span.enter();

                let blob_reader = self.blob_service.open_read(blob_digest).await;

                match blob_reader {
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
                            .insert(fh, Arc::new(tokio::sync::Mutex::new(blob_reader)));

                        Ok((
                            Some(fh),
                            fuse_backend_rs::api::filesystem::OpenOptions::empty(),
                        ))
                    }
                }
            }
        }
    }

    #[tracing::instrument(skip_all, fields(rq.inode = _inode, rq.offset = offset, rq.size = size))]
    async fn async_read(
        &self,
        _ctx: &Context,
        _inode: Self::Inode,
        handle: Self::Handle,
        w: &mut (dyn AsyncZeroCopyWriter + Send),
        size: u32,
        offset: u64,
        _lock_owner: Option<u64>,
        _flags: u32,
    ) -> io::Result<usize> {
        debug!("read");

        // We need to take out the blob reader from self.file_handles, so we can
        // interact with it in the separate task.
        // On success, we pass it back out of the task, so we can put it back in self.file_handles.
        let blob_reader = match self.file_handles.read().get(&handle) {
            Some(blob_reader) => blob_reader.clone(),
            None => {
                warn!("file handle {} unknown", handle);
                return Err(io::Error::from_raw_os_error(libc::EIO));
            }
        };

        let mut blob_reader = blob_reader.lock().await;

        // seek to the offset specified, which is relative to the start of the file.
        let resp = blob_reader.seek(io::SeekFrom::Start(offset)).await;

        match resp {
            Ok(pos) => {
                debug_assert_eq!(offset, pos);
            }
            Err(e) => {
                warn!("failed to seek to offset {}: {}", offset, e);
                return Err(io::Error::from_raw_os_error(libc::EIO));
            }
        }

        // As written in the fuse docs, read should send exactly the number
        // of bytes requested except on EOF or error.

        let mut buf: Vec<u8> = Vec::with_capacity(size as usize);

        // copy things from the internal buffer into buf to fill it till up until size
        tokio::io::copy(&mut blob_reader.as_mut().take(size as u64), &mut buf).await?;

        w.async_write(&buf).await
    }

    #[tracing::instrument(skip_all, fields(rq.inode = inode, rq.offset = offset))]
    fn async_readdir<'a, 'b, 'async_trait>(
        &'a self,
        _ctx: &'b Context,
        inode: Self::Inode,
        _handle: Self::Handle,
        _size: u32,
        offset: u64,
    ) -> BoxStream<'async_trait, io::Result<OwnedDirEntry>>
    where
        'a: 'async_trait,
        'b: 'async_trait,
        Self: 'async_trait,
    {
        debug!("readdir");

        Box::pin(try_stream! {
            if inode == ROOT_ID {
                if !self.list_root {
                    Err(io::Error::from_raw_os_error(libc::EPERM))?; // same error code as ipfs/kubo
                } else {
                    let mut stream = self
                        .root_nodes_provider
                        .list()
                        .skip(offset as usize)
                        .enumerate();

                    while let Some((i, ref root_node)) = stream.next().await {
                        let root_node = root_node.as_ref().map_err(|e| {
                            warn!("failed to retrieve pathinfo: {}", e);
                            io::Error::from_raw_os_error(libc::EPERM)
                        })?;

                        let name = root_node.get_name();
                        // obtain the inode, or allocate a new one.
                        let ino = self.get_inode_for_root_name(name).unwrap_or_else(|| {
                            // insert the (sparse) inode data and register in
                            // self.root_nodes.
                            let ino = self.inode_tracker.write().put(root_node.into());
                            self.root_nodes.write().insert(name.into(), ino);
                            ino
                        });

                        let ty = match root_node {
                            Node::Directory(_) => libc::S_IFDIR,
                            Node::File(_) => libc::S_IFREG,
                            Node::Symlink(_) => libc::S_IFLNK,
                        };

                        yield fuse_backend_rs::api::filesystem::OwnedDirEntry {
                            ino,
                            offset: offset + i as u64 + 1,
                            type_: ty,
                            name: name.into(),
                        };
                    }

                    return;
                }
            }

            // lookup the children, or return an error if it's not a directory.
            let (parent_digest, children) = self.get_directory_children(inode).await?;

            // TODO(cbrewster): Spans like this are not async compatible
            let span = info_span!("lookup", directory.digest = %parent_digest);
            let _enter = span.enter();

            for (i, (ino, child_node)) in children.iter().skip(offset as usize).enumerate() {
                // the second parameter will become the "offset" parameter on the next call.
                yield fuse_backend_rs::api::filesystem::OwnedDirEntry {
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
                    name: child_node.get_name().into(),
                };
            }
        })
    }

    // TODO: readdirplus?
    fn async_readdirplus<'a, 'b, 'async_trait>(
        &'a self,
        _ctx: &'b Context,
        _inode: Self::Inode,
        _handle: Self::Handle,
        _size: u32,
        _offset: u64,
    ) -> BoxStream<'async_trait, io::Result<(OwnedDirEntry, Entry)>>
    where
        'a: 'async_trait,
        'b: 'async_trait,
        Self: 'async_trait,
    {
        todo!()
    }

    async fn async_setattr(
        &self,
        _ctx: &Context,
        _inode: Self::Inode,
        _attr: stat64,
        _handle: Option<Self::Handle>,
        _valid: SetattrValid,
    ) -> io::Result<(stat64, Duration)> {
        Err(io::Error::from_raw_os_error(libc::ENOSYS))
    }

    async fn async_create(
        &self,
        _ctx: &Context,
        _parent: Self::Inode,
        _name: &CStr,
        _args: CreateIn,
    ) -> io::Result<(Entry, Option<Self::Handle>, OpenOptions)> {
        Err(io::Error::from_raw_os_error(libc::ENOSYS))
    }

    async fn async_write(
        &self,
        _ctx: &Context,
        _inode: Self::Inode,
        _handle: Self::Handle,
        _r: &mut (dyn AsyncZeroCopyReader + Send),
        _size: u32,
        _offset: u64,
        _lock_owner: Option<u64>,
        _delayed_write: bool,
        _flags: u32,
        _fuse_flags: u32,
    ) -> io::Result<usize> {
        Err(io::Error::from_raw_os_error(libc::ENOSYS))
    }

    async fn async_fsync(
        &self,
        _ctx: &Context,
        _inode: Self::Inode,
        _datasync: bool,
        _handle: Self::Handle,
    ) -> io::Result<()> {
        Err(io::Error::from_raw_os_error(libc::ENOSYS))
    }

    async fn async_fallocate(
        &self,
        _ctx: &Context,
        _inode: Self::Inode,
        _handle: Self::Handle,
        _mode: u32,
        _offset: u64,
        _length: u64,
    ) -> io::Result<()> {
        Err(io::Error::from_raw_os_error(libc::ENOSYS))
    }

    async fn async_fsyncdir(
        &self,
        _ctx: &Context,
        _inode: Self::Inode,
        _datasync: bool,
        _handle: Self::Handle,
    ) -> io::Result<()> {
        Err(io::Error::from_raw_os_error(libc::ENOSYS))
    }
}
