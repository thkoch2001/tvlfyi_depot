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
use futures::FutureExt;
use futures::TryStreamExt;
use tokio::io::AsyncSeek;

#[derive(Debug)]
struct BlobRef {
    digest: B3Digest,
    size: u64,
}

#[derive(Debug)]
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

/// Implements AsyncSeek and AsyncRead for a Blob which may or may not be opened
enum BlobFuture {
    Opening {
        fut: BoxFuture<'static, io::Result<Option<Box<dyn BlobReader>>>>,
        digest: B3Digest,
        /// Where we will seek after the Blob has been opened
        seek_target: Option<io::SeekFrom>,
    },
    Seeking(Box<dyn BlobReader>),
    Reading(Box<dyn BlobReader>),
    Failed,
}

impl BlobFuture {
    fn new(blob_service: Arc<impl BlobService + 'static>, r: &BlobRef) -> Self {
        let digest = r.digest.clone();
        Self::Opening {
            fut: (async move { blob_service.open_read(&digest).await }).boxed(),
            digest: r.digest.clone(),
            seek_target: None,
        }
    }
}

impl tokio::io::AsyncSeek for BlobFuture {
    fn start_seek(mut self: Pin<&mut Self>, pos: io::SeekFrom) -> io::Result<()> {
        match std::mem::replace(&mut *self, BlobFuture::Failed) {
            Self::Opening {
                fut,
                digest,
                seek_target,
            } => match seek_target {
                Some(pos) => {
                    *self = Self::Opening {
                        fut,
                        digest,
                        seek_target: Some(pos),
                    };
                    Err(io::Error::new(
                        io::ErrorKind::Other,
                        "Seek already in progress",
                    ))
                }
                None => {
                    *self = Self::Opening {
                        fut,
                        digest,
                        seek_target: Some(pos),
                    };
                    Ok(())
                }
            },
            Self::Seeking(reader) => {
                *self = Self::Seeking(reader);
                Err(io::Error::new(
                    io::ErrorKind::Other,
                    "Seek already in progress",
                ))
            }
            Self::Reading(mut reader) => match Pin::new(&mut reader).start_seek(pos) {
                Ok(()) => {
                    *self = Self::Seeking(reader);
                    Ok(())
                }
                Err(e) => {
                    *self = Self::Reading(reader);
                    Err(e)
                }
            },
            Self::Failed => Err(io::Error::new(io::ErrorKind::Other, "Bad blob")),
        }
    }
    fn poll_complete(mut self: Pin<&mut Self>, cx: &mut Context) -> Poll<io::Result<u64>> {
        match std::mem::replace(&mut *self, BlobFuture::Failed) {
            Self::Opening {
                mut fut,
                digest,
                seek_target,
            } => {
                match Pin::new(&mut fut).poll(cx) {
                    Poll::Pending => {
                        *self = Self::Opening {
                            fut,
                            digest,
                            seek_target,
                        };
                        Poll::Pending
                    }
                    Poll::Ready(res) => {
                        // If an error is returned, the future will remain in the Failed state
                        let mut reader = res?.ok_or(io::Error::new(
                            io::ErrorKind::NotFound,
                            RenderError::BlobNotFound(digest.clone(), Default::default()),
                        ))?;
                        if let Some(pos) = seek_target {
                            Pin::new(&mut reader).start_seek(pos)?; // same here
                        }
                        *self = Self::Seeking(reader);
                        self.poll_complete(cx)
                    }
                }
            }
            Self::Seeking(mut reader) => match Pin::new(&mut reader).poll_complete(cx) {
                Poll::Pending => {
                    *self = Self::Seeking(reader);
                    Poll::Pending
                }
                Poll::Ready(res) => {
                    let res = res?;
                    *self = Self::Reading(reader);
                    Poll::Ready(Ok(res))
                }
            },
            Self::Reading(mut reader) => {
                let res = Pin::new(&mut reader).poll_complete(cx);
                *self = Self::Reading(reader);
                res
            }
            Self::Failed => Poll::Ready(Err(io::Error::new(io::ErrorKind::Other, "Bad blob"))),
        }
    }
}

impl tokio::io::AsyncRead for BlobFuture {
    fn poll_read(
        mut self: Pin<&mut Self>,
        cx: &mut Context,
        buf: &mut tokio::io::ReadBuf,
    ) -> Poll<io::Result<()>> {
        match std::mem::replace(&mut *self, BlobFuture::Failed) {
            Self::Opening {
                mut fut,
                digest,
                seek_target,
            } => {
                match Pin::new(&mut fut).poll(cx) {
                    Poll::Pending => {
                        *self = Self::Opening {
                            fut,
                            digest,
                            seek_target,
                        };
                        Poll::Pending
                    }
                    Poll::Ready(res) => {
                        // If an error is returned, the future will remain in the Failed state
                        let mut reader = res?.ok_or(io::Error::new(
                            io::ErrorKind::NotFound,
                            RenderError::BlobNotFound(digest.clone(), Default::default()),
                        ))?;
                        if let Some(pos) = seek_target {
                            Pin::new(&mut reader).start_seek(pos)?; // same here
                        }
                        *self = Self::Seeking(reader);
                        futures::ready!(self.as_mut().poll_complete(cx))?;
                        self.poll_read(cx, buf)
                    }
                }
            }
            Self::Seeking(mut reader) => match Pin::new(&mut reader).poll_complete(cx) {
                Poll::Pending => {
                    *self = Self::Seeking(reader);
                    Poll::Pending
                }
                Poll::Ready(res) => {
                    let _ = res?;
                    *self = Self::Reading(reader);
                    self.poll_read(cx, buf)
                }
            },
            Self::Reading(mut reader) => {
                let res = Pin::new(&mut reader).poll_read(cx, buf);
                *self = Self::Reading(reader);
                res
            }
            Self::Failed => Poll::Ready(Err(io::Error::new(io::ErrorKind::Other, "Bad blob"))),
        }
    }
}

pub struct Reader<B: BlobService> {
    segments: Vec<(u64, Data)>,
    position_bytes: u64,
    position_index: usize,
    blob_service: Arc<B>,
    current_blob: Option<BlobFuture>,
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
        root_node: Node,
        blob_service: B,
        directory_service: impl DirectoryService,
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

        Self::new_with_directory_closure(root_node, blob_service, maybe_directory_closure)
    }

    /// Creates a new seekable NAR renderer for the given castore root node.
    /// This version of the instantiation does not perform any I/O and as such is not async.
    /// However it requires all directories to be passed as a ValidatedDirectoryGraph.
    ///
    /// panics if the directory closure is not the closure of the root node
    pub fn new_with_directory_closure(
        root_node: Node,
        blob_service: B,
        directory_closure: Option<ValidatedDirectoryGraph>,
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
            current_blob: Default::default(),
        })
    }

    pub fn stream_len(&self) -> u64 {
        self.segments
            .last()
            .map(|&(off, ref data)| off + data.len())
            .expect("no segments found")
    }
}

impl<B: BlobService + 'static> tokio::io::AsyncSeek for Reader<B> {
    fn start_seek(mut self: Pin<&mut Self>, pos: io::SeekFrom) -> io::Result<()> {
        let stream_len = Reader::stream_len(&self);

        let this = &mut *self;

        // TODO(edef): be sane about overflows
        let pos = match pos {
            io::SeekFrom::Start(n) => n,
            io::SeekFrom::End(n) => (stream_len as i64 + n) as u64,
            io::SeekFrom::Current(n) => (this.position_bytes as i64 + n) as u64,
        };

        let prev_position_bytes = this.position_bytes;
        let prev_position_index = this.position_index;

        this.position_bytes = min(pos, stream_len);
        this.position_index = match this
            .segments
            .binary_search_by_key(&this.position_bytes, |&(off, _)| off)
        {
            Ok(idx) => idx,
            Err(idx) => idx - 1,
        };

        let Some((offset, Data::Blob(blobref))) = this.segments.get(this.position_index) else {
            // If not seeking into a blob, we clear the active blob reader and then we're done
            this.current_blob = Default::default();
            return Ok(());
        };
        let offset_in_segment = this.position_bytes - offset;

        if prev_position_bytes == this.position_bytes {
            // position has not changed. do nothing
        } else if prev_position_index == this.position_index {
            // seeking within the same segment, re-use the blob reader
            let Some(blob) = &mut self.current_blob else {
                unreachable!()
            };
            Pin::new(blob).start_seek(io::SeekFrom::Start(offset_in_segment))?;
        } else {
            // seek to a different segment
            let mut new_blob = BlobFuture::new(this.blob_service.clone(), blobref);
            if offset_in_segment != 0 {
                Pin::new(&mut new_blob).start_seek(io::SeekFrom::Start(offset_in_segment))?;
            }
            this.current_blob = Some(new_blob);
        };

        Ok(())
    }
    fn poll_complete(mut self: Pin<&mut Self>, cx: &mut Context) -> Poll<io::Result<u64>> {
        let this = &mut *self;

        if let Some(blob) = &mut this.current_blob {
            futures::ready!(Pin::new(blob).poll_complete(cx))?;
        }

        Poll::Ready(Ok(this.position_bytes))
    }
}

impl<B: BlobService + 'static> tokio::io::AsyncRead for Reader<B> {
    fn poll_read(
        mut self: Pin<&mut Self>,
        cx: &mut Context,
        buf: &mut tokio::io::ReadBuf,
    ) -> Poll<io::Result<()>> {
        let this = &mut *self;

        let Some(&(offset, ref segment)) = this.segments.get(this.position_index) else {
            return Poll::Ready(Ok(())); // EOF
        };

        let prev_read_buf_pos = buf.filled().len();
        match segment {
            Data::Literal(data) => {
                let offset_in_segment = this.position_bytes - offset;
                let offset_in_segment = usize::try_from(offset_in_segment).unwrap();
                let remaining_data = data.len() - offset_in_segment;
                let read_size = std::cmp::min(remaining_data, buf.remaining());
                buf.put(&data[offset_in_segment..offset_in_segment + read_size]);
            }
            Data::Blob(BlobRef { .. }) => {
                let Some(blob) = &mut this.current_blob else {
                    unreachable!()
                };
                futures::ready!(Pin::new(blob).poll_read(cx, buf))?;
            }
        };
        let new_read_buf_pos = buf.filled().len();
        this.position_bytes += (new_read_buf_pos - prev_read_buf_pos) as u64;

        let prev_position_index = this.position_index;
        while {
            if let Some(&(offset, ref segment)) = this.segments.get(this.position_index) {
                (this.position_bytes - offset) >= segment.len()
            } else {
                false
            }
        } {
            this.position_index += 1;
        }
        if prev_position_index != this.position_index {
            let Some((_offset, Data::Blob(blobref))) = this.segments.get(this.position_index)
            else {
                // If the next segment is not a blob, we clear the active blob reader and then we're done
                this.current_blob = Default::default();
                return Poll::Ready(Ok(()));
            };

            this.current_blob = Some(BlobFuture::new(this.blob_service.clone(), blobref));
        }

        Poll::Ready(Ok(()))
    }
}
