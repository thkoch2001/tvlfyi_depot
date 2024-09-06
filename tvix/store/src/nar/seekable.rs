use std::{
    cmp::min,
    io::{self, Read},
};

use super::RenderError;

use bytes::{Buf, Bytes};

use nix_compat::nar::writer::interruptable as nar_writer;
use tvix_castore::directoryservice::{
    DirectoryGraph, DirectoryService, RootToLeavesValidator, ValidatedDirectoryGraph,
};
use tvix_castore::Directory;
use tvix_castore::{B3Digest, Node};

use futures::TryStreamExt;

#[derive(Debug)]
pub struct BlobRef {
    digest: B3Digest,
    size: u64,
}

impl BlobRef {
    fn read_at(&self, offset: u64, buf: &mut [u8]) -> io::Result<usize> {
        todo!()
    }
}

#[derive(Debug)]
pub enum Data {
    Literal(Bytes),
    Blob(BlobRef),
}

impl Data {
    pub fn len(&self) -> u64 {
        match self {
            Data::Literal(buf) => buf.len() as u64,
            Data::Blob(BlobRef { size, .. }) => *size,
        }
    }

    fn read_at(&self, offset: u64, buf: &mut [u8]) -> io::Result<usize> {
        match self {
            Data::Literal(data) => {
                let offset = usize::try_from(offset).unwrap();
                data.slice(offset..).reader().read(buf)
            }
            Data::Blob(data) => data.read_at(offset, buf),
        }
    }
}

#[derive(Debug)]
pub struct Reader {
    segments: Vec<(u64, Data)>,
    position_bytes: u64,
    position_index: usize,
}

/// Used during construction
fn walk_node(
    reader: &mut Reader,
    offset: &mut u64,
    // MUST be sorted by digest and contain all directories referenced by node
    directories: &Vec<(B3Digest, Directory)>,
    node: Node,
    nar_node: nar_writer::Node<'_, Vec<u8>>,
) -> Result<(), RenderError> {
    match node {
        tvix_castore::Node::Directory { digest, .. } => {
            let directory = directories
                .binary_search_by_key(&digest.as_slice(), |(digest, directory)| &digest.as_slice())
                .map(|pos| directories[pos].clone())
                .unwrap()
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

                walk_node(reader, offset, directories, node.clone(), child_node)?;
            }
        }
        tvix_castore::Node::File {
            digest,
            size,
            executable,
        } => {
            let (cur_segment, skip) = nar_node
                .file_skip(executable, size)
                .map_err(RenderError::NARWriterError)?;

            // Flush the segment up until the beginning of the blob
            let segment_size = cur_segment.len();
            reader
                .segments
                .push((*offset, Data::Literal(std::mem::take(cur_segment).into())));
            *offset += segment_size as u64;

            reader
                .segments
                .push((*offset, Data::Blob(BlobRef { digest, size })));
            *offset += size;

            // This writes the padding and appropriate tokens to the next segment after the blob
            let nar_node = skip.cont(cur_segment);
        }
        tvix_castore::Node::Symlink { target } => {
            nar_node
                .symlink(target.as_ref())
                .map_err(RenderError::NARWriterError)?;
        }
    }
    Ok(())
}

impl Reader {
    pub async fn new(
        directory_service: impl DirectoryService,
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

        Ok(Self::new_with_directory_closure(
            maybe_directory_closure,
            root_node,
        )?)
    }

    /// panics if the directory closure is not the closure of the root node
    pub fn new_with_directory_closure(
        directory_closure: Option<ValidatedDirectoryGraph>,
        root_node: Node,
    ) -> Result<Self, RenderError> {
        let mut reader = Self {
            segments: vec![],
            position_bytes: 0,
            position_index: 0,
        };

        let directories = directory_closure
            .map(|directory_closure| {
                directory_closure
                    .drain_root_to_leaves()
                    .map(|dir| (dir.digest(), dir))
                    .collect()
            })
            .unwrap_or_default();

        let mut cur_segment: Vec<u8> = vec![];
        let mut offset = 0;

        let nar_node = nar_writer::open(&mut cur_segment).map_err(RenderError::NARWriterError)?;

        walk_node(&mut reader, &mut offset, &directories, root_node, nar_node)?;

        Ok(reader)
    }

    pub fn stream_len(&self) -> u64 {
        self.segments
            .last()
            .map(|&(off, ref data)| off + data.len())
            .unwrap_or_default()
    }
}

impl io::Seek for Reader {
    fn seek(&mut self, pos: io::SeekFrom) -> io::Result<u64> {
        let stream_len = Reader::stream_len(self);

        // TODO(edef): be sane about overflows
        let pos = match pos {
            io::SeekFrom::Start(n) => n,
            io::SeekFrom::End(n) => (stream_len as i64 + n) as u64,
            io::SeekFrom::Current(n) => (self.position_bytes as i64 + n) as u64,
        };

        self.position_bytes = min(pos, stream_len);
        self.position_index = match self
            .segments
            .binary_search_by_key(&self.position_bytes, |&(off, _)| off)
        {
            Ok(idx) => idx,
            Err(idx) => idx - 1,
        };

        Ok(pos)
    }

    fn rewind(&mut self) -> io::Result<()> {
        self.position_bytes = 0;
        self.position_index = 0;
        Ok(())
    }

    fn stream_position(&mut self) -> io::Result<u64> {
        Ok(self.position_bytes)
    }
}

impl io::Read for Reader {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        let Some(&(offset, ref data)) = self.segments.get(self.position_index) else {
            return Ok(0); // EOF
        };

        let n = data.read_at(self.position_bytes - offset, buf)?;
        self.position_bytes += n as u64;

        if (self.position_bytes - offset) >= data.len() {
            self.position_index += 1;
        }

        Ok(n)
    }
}
