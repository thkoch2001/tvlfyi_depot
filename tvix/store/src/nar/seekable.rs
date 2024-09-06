use std::{
    cmp::min,
    future::Future,
    io,
    pin::Pin,
    sync::Arc,
    task::{Context, Poll},
};

use super::RenderError;

use bytes::{BufMut, Bytes};

use nix_compat::nar::writer::sync as nar_writer;
use tvix_castore::blobservice::{BlobReader, BlobService};
use tvix_castore::directoryservice::{
    DirectoryGraph, DirectoryService, RootToLeavesValidator, ValidatedDirectoryGraph,
};
use tvix_castore::Directory;
use tvix_castore::{B3Digest, Node};

use futures::future::BoxFuture;
use futures::future::TryMaybeDone;
use futures::FutureExt;
use futures::TryStreamExt;

use pin_project_lite::pin_project;
use tokio::io::AsyncSeekExt;

struct BlobRef {
    digest: B3Digest,
    size: u64,
}

enum Data {
    Literal(Bytes),
    Blob(BlobRef),
}

impl Data {
    pub fn len(&self) -> u64 {
        match self {
            Data::Literal(data) => data.len() as u64,
            Data::Blob(BlobRef { size, .. }) => *size,
        }
    }
}

pin_project! {
    pub struct Reader<B: BlobService> {
        segments: Vec<(u64, Data)>,
        position_bytes: u64,
        position_index: usize,
        blob_service: Arc<B>,
        #[pin]
        current_blob: TryMaybeDone<BoxFuture<'static, io::Result<Box<dyn BlobReader>>>>,
    }
}

/// Used during construction.
/// Converts the current buffer (passed as `cur_segment`) into a `Data::Literal` segment and
/// inserts it into `self.segments`.
fn flush_segment(segments: &mut Vec<(u64, Data)>, offset: &mut u64, cur_segment: &mut Vec<u8>) {
    let segment_size = cur_segment.len();
    segments.push((*offset, Data::Literal(std::mem::take(cur_segment).into())));
    *offset += segment_size as u64;
}

/// Used during construction.
/// Recursively walks the node and its children, and fills `segments` with the appropriate
/// `Data::Literal` and `Data::Blob` elements.
fn walk_node(
    segments: &mut Vec<(u64, Data)>,
    offset: &mut u64,
    // MUST be sorted by digest and contain all directories referenced by node
    directories: &Vec<(B3Digest, Directory)>,
    node: Node,
    // Includes a reference to the current segment's buffer
    nar_node: nar_writer::Node<'_, Vec<u8>>,
) -> Result<(), RenderError> {
    match node {
        tvix_castore::Node::Symlink { target } => {
            nar_node
                .symlink(target.as_ref())
                .map_err(RenderError::NARWriterError)?;
        }
        tvix_castore::Node::File {
            digest,
            size,
            executable,
        } => {
            let (cur_segment, skip) = nar_node
                .file_manual_write(executable, size)
                .map_err(RenderError::NARWriterError)?;

            // Flush the segment up until the beginning of the blob
            flush_segment(segments, offset, cur_segment);

            // Insert the blob segment
            segments.push((*offset, Data::Blob(BlobRef { digest, size })));
            *offset += size;

            // Close the file node
            skip.close(cur_segment)
                .map_err(RenderError::NARWriterError)?;
        }
        tvix_castore::Node::Directory { digest, .. } => {
            let directory = directories
                .binary_search_by_key(&digest.as_slice(), |(digest, _dir)| digest.as_slice())
                .map(|pos| directories[pos].clone())
                .expect("missing directory") // DirectoryGraph checks this
                .1;

            // start a directory node
            let mut nar_node_directory =
                nar_node.directory().map_err(RenderError::NARWriterError)?;

            // for each node in the directory, create a new entry with its name,
            // and then recurse on that entry.
            for (name, node) in directory.nodes() {
                let child_node = nar_node_directory
                    .entry(name.as_ref())
                    .map_err(RenderError::NARWriterError)?;

                walk_node(segments, offset, directories, node.clone(), child_node)?;
            }

            // close the directory
            nar_node_directory
                .close()
                .map_err(RenderError::NARWriterError)?;
        }
    }
    Ok(())
}

impl<B: BlobService + 'static> Reader<B> {
    /// Creates a new seekable NAR renderer for the given castore root node.
    pub async fn new(
        directory_service: impl DirectoryService,
        blob_service: B,
        root_node: Node,
    ) -> Result<Self, RenderError> {
        let maybe_directory_closure = match &root_node {
            // If this is a directory, resolve all subdirectories
            Node::Directory { digest, .. } => {
                let mut closure = DirectoryGraph::with_order(
                    RootToLeavesValidator::new_with_root_digest(digest.clone()),
                );
                let mut stream = directory_service.get_recursive(digest);
                while let Some(dir) = stream
                    .try_next()
                    .await
                    .map_err(|e| RenderError::StoreError(e.into()))?
                {
                    closure.add(dir).map_err(|e| {
                        RenderError::StoreError(
                            tvix_castore::Error::StorageError(e.to_string()).into(),
                        )
                    })?;
                }
                Some(closure.validate().map_err(|e| {
                    RenderError::StoreError(tvix_castore::Error::StorageError(e.to_string()).into())
                })?)
            }
            // If the top-level node is a file or a symlink, just pass it on
            Node::File { .. } => None,
            Node::Symlink { .. } => None,
        };

        Self::new_with_directory_closure(maybe_directory_closure, blob_service, root_node)
    }

    /// Creates a new seekable NAR renderer for the given castore root node.
    /// This version of the instantiation does not perform any I/O and as such is not async.
    /// However it requires all directories to be passed as a ValidatedDirectoryGraph.
    ///
    /// panics if the directory closure is not the closure of the root node
    pub fn new_with_directory_closure(
        directory_closure: Option<ValidatedDirectoryGraph>,
        blob_service: B,
        root_node: Node,
    ) -> Result<Self, RenderError> {
        let directories = directory_closure
            .map(|directory_closure| {
                let mut directories: Vec<(B3Digest, Directory)> = vec![];
                for dir in directory_closure.drain_root_to_leaves() {
                    let digest = dir.digest();
                    let pos = directories
                        .binary_search_by_key(&digest.as_slice(), |(digest, _dir)| {
                            digest.as_slice()
                        })
                        .expect_err("duplicate directory"); // DirectoryGraph checks this
                    directories.insert(pos, (digest, dir));
                }
                directories
            })
            .unwrap_or_default();

        let mut segments = vec![];
        let mut cur_segment: Vec<u8> = vec![];
        let mut offset = 0;

        let nar_node = nar_writer::open(&mut cur_segment).map_err(RenderError::NARWriterError)?;

        walk_node(
            &mut segments,
            &mut offset,
            &directories,
            root_node,
            nar_node,
        )?;
        flush_segment(&mut segments, &mut offset, &mut cur_segment);

        Ok(Reader {
            segments,
            position_bytes: 0,
            position_index: 0,
            blob_service: blob_service.into(),
            current_blob: TryMaybeDone::Gone,
        })
    }

    pub fn stream_len(&self) -> u64 {
        self.segments
            .last()
            .map(|&(off, ref data)| off + data.len())
            .unwrap_or_default()
    }
}

impl<B: BlobService + 'static> tokio::io::AsyncSeek for Reader<B> {
    fn start_seek(self: Pin<&mut Self>, pos: io::SeekFrom) -> io::Result<()> {
        let stream_len = Reader::stream_len(&self);

        let mut this = self.project();

        // TODO(edef): be sane about overflows
        let pos = match pos {
            io::SeekFrom::Start(n) => n,
            io::SeekFrom::End(n) => (stream_len as i64 + n) as u64,
            io::SeekFrom::Current(n) => (*this.position_bytes as i64 + n) as u64,
        };

        let prev_position_bytes = *this.position_bytes;
        let prev_position_index = *this.position_index;

        *this.position_bytes = min(pos, stream_len);
        *this.position_index = match this
            .segments
            .binary_search_by_key(this.position_bytes, |&(off, _)| off)
        {
            Ok(idx) => idx,
            Err(idx) => idx - 1,
        };

        let Some((offset, Data::Blob(BlobRef { digest, .. }))) =
            this.segments.get(*this.position_index)
        else {
            // If not seeking into a blob, we clear the active blob reader and then we're done
            *this.current_blob = TryMaybeDone::Gone;
            return Ok(());
        };
        let offset_in_segment = *this.position_bytes - offset;

        if prev_position_bytes == *this.position_bytes {
            // position has not changed. do nothing
        } else if prev_position_index == *this.position_index {
            use std::ops::DerefMut;
            // seeking within the same segment, and there is an active blob reader
            let prev = std::mem::replace(this.current_blob.deref_mut(), TryMaybeDone::Gone);
            *this.current_blob = futures::future::try_maybe_done(
                (async move {
                    let mut pinned = std::pin::pin!(prev);
                    pinned.as_mut().await?;
                    let mut reader = pinned.take_output().unwrap();
                    reader.seek(io::SeekFrom::Start(offset_in_segment)).await?;
                    Ok(reader)
                })
                .boxed(),
            );
        } else {
            // seek to a different segment
            let blob_service = this.blob_service.clone();
            let digest = digest.clone();
            *this.current_blob = futures::future::try_maybe_done(
                (async move {
                    let mut reader =
                        blob_service
                            .open_read(&digest)
                            .await?
                            .ok_or(io::Error::new(
                                io::ErrorKind::Other,
                                RenderError::BlobNotFound(digest.clone(), Default::default()),
                            ))?;
                    if offset_in_segment != 0 {
                        reader.seek(io::SeekFrom::Start(offset_in_segment)).await?;
                    }
                    Ok(reader)
                })
                .boxed(),
            );
        };

        Ok(())
    }
    fn poll_complete(self: Pin<&mut Self>, cx: &mut Context) -> Poll<io::Result<u64>> {
        let this = self.project();

        futures::ready!(this.current_blob.poll(cx))?;

        Poll::Ready(Ok(*this.position_bytes))
    }
}

impl<B: BlobService + 'static> tokio::io::AsyncRead for Reader<B> {
    fn poll_read(
        self: Pin<&mut Self>,
        cx: &mut Context,
        buf: &mut tokio::io::ReadBuf,
    ) -> Poll<io::Result<()>> {
        let mut this = self.project();

        let Some(&(offset, ref segment)) = this.segments.get(*this.position_index) else {
            return Poll::Ready(Ok(())); // EOF
        };

        let prev_read_buf_pos = buf.filled().len();
        match segment {
            Data::Literal(data) => {
                let offset_in_segment = *this.position_bytes - offset;
                let offset_in_segment = usize::try_from(offset_in_segment).unwrap();
                buf.put(data.slice(offset_in_segment..));
                return Poll::Ready(Ok(()));
            }
            Data::Blob(BlobRef { digest, .. }) => {
                use std::ops::DerefMut;
                let current_blob = this.current_blob.deref_mut();
                if let TryMaybeDone::Gone = current_blob {
                    let blob_service = this.blob_service.clone();
                    let digest = digest.clone();
                    *current_blob =
                        futures::future::try_maybe_done(
                            (async move {
                                let reader = blob_service.open_read(&digest).await?.ok_or(
                                    io::Error::new(
                                        io::ErrorKind::Other,
                                        RenderError::BlobNotFound(
                                            digest.clone(),
                                            Default::default(),
                                        ),
                                    ),
                                )?;
                                Ok(reader)
                            })
                            .boxed(),
                        );
                }
                futures::ready!(this.current_blob.as_mut().poll(cx))?;
                let reader = this.current_blob.as_mut().output_mut().unwrap();
                let pinned = std::pin::pin!(reader);
                futures::ready!(pinned.poll_read(cx, buf))?;
            }
        };
        let new_read_buf_pos = buf.filled().len();
        *this.position_bytes += (new_read_buf_pos - prev_read_buf_pos) as u64;

        if (*this.position_bytes - offset) >= segment.len() {
            *this.position_index += 1;
            *this.current_blob = TryMaybeDone::Gone;
        }

        Poll::Ready(Ok(()))
    }
}
