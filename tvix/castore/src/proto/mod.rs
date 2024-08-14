use std::str;

use prost::Message;

mod grpc_blobservice_wrapper;
mod grpc_directoryservice_wrapper;

use crate::{B3Digest, DirectoryError};
pub use grpc_blobservice_wrapper::GRPCBlobServiceWrapper;
pub use grpc_directoryservice_wrapper::GRPCDirectoryServiceWrapper;

tonic::include_proto!("tvix.castore.v1");

#[cfg(feature = "tonic-reflection")]
/// Compiled file descriptors for implementing [gRPC
/// reflection](https://github.com/grpc/grpc/blob/master/doc/server-reflection.md) with e.g.
/// [`tonic_reflection`](https://docs.rs/tonic-reflection).
pub const FILE_DESCRIPTOR_SET: &[u8] = tonic::include_file_descriptor_set!("tvix.castore.v1");

#[cfg(test)]
mod tests;

/// Errors that occur during StatBlobResponse validation
#[derive(Debug, PartialEq, Eq, thiserror::Error)]
pub enum ValidateStatBlobResponseError {
    /// Invalid digest length encountered
    #[error("Invalid digest length {0} for chunk #{1}")]
    InvalidDigestLen(usize, usize),
}

fn checked_sum(iter: impl IntoIterator<Item = u64>) -> Option<u64> {
    iter.into_iter().try_fold(0u64, |acc, i| acc.checked_add(i))
}

impl Directory {
    /// The size of a directory is the number of all regular and symlink elements,
    /// the number of directory elements, and their size fields.
    pub fn size(&self) -> u64 {
        if cfg!(debug_assertions) {
            self.size_checked()
                .expect("Directory::size exceeds u64::MAX")
        } else {
            self.size_checked().unwrap_or(u64::MAX)
        }
    }

    fn size_checked(&self) -> Option<u64> {
        checked_sum([
            self.files.len().try_into().ok()?,
            self.symlinks.len().try_into().ok()?,
            self.directories.len().try_into().ok()?,
            checked_sum(self.directories.iter().map(|e| e.size))?,
        ])
    }

    /// Calculates the digest of a Directory, which is the blake3 hash of a
    /// Directory protobuf message, serialized in protobuf canonical form.
    pub fn digest(&self) -> B3Digest {
        let mut hasher = blake3::Hasher::new();

        hasher
            .update(&self.encode_to_vec())
            .finalize()
            .as_bytes()
            .into()
    }
}

/// Accepts a name, and a mutable reference to the previous name.
/// If the passed name is larger than the previous one, the reference is updated.
/// If it's not, an error is returned.
fn update_if_lt_prev<'n>(prev_name: &mut &'n [u8], name: &'n [u8]) -> Result<(), DirectoryError> {
    if *name < **prev_name {
        return Err(DirectoryError::WrongSorting(bytes::Bytes::copy_from_slice(
            name,
        )));
    }
    *prev_name = name;
    Ok(())
}

// TODO: add a proper owned version here that moves various fields
impl TryFrom<Directory> for crate::Directory {
    type Error = DirectoryError;

    fn try_from(value: Directory) -> Result<Self, Self::Error> {
        (&value).try_into()
    }
}

impl TryFrom<&Directory> for crate::Directory {
    type Error = DirectoryError;

    fn try_from(directory: &Directory) -> Result<crate::Directory, DirectoryError> {
        let mut dir = crate::Directory::new();

        let mut last_file_name: &[u8] = b"";

        // TODO: this currently loops over all three types separately, rather
        // than peeking and picking from where would be the next.

        for file in directory.files.iter().map(move |file| {
            update_if_lt_prev(&mut last_file_name, &file.name).map(|()| file.clone())
        }) {
            let file = file?;

            let (name, node) = Node {
                node: Some(node::Node::File(file)),
            }
            .into_name_and_node()?;

            dir.add(name, node)?;
        }
        let mut last_directory_name: &[u8] = b"";
        for directory in directory.directories.iter().map(move |directory| {
            update_if_lt_prev(&mut last_directory_name, &directory.name).map(|()| directory.clone())
        }) {
            let directory = directory?;

            let (name, node) = Node {
                node: Some(node::Node::Directory(directory)),
            }
            .into_name_and_node()?;

            dir.add(name, node)?;
        }
        let mut last_symlink_name: &[u8] = b"";
        for symlink in directory.symlinks.iter().map(move |symlink| {
            update_if_lt_prev(&mut last_symlink_name, &symlink.name).map(|()| symlink.clone())
        }) {
            let symlink = symlink?;

            let (name, node) = Node {
                node: Some(node::Node::Symlink(symlink)),
            }
            .into_name_and_node()?;

            dir.add(name, node)?;
        }

        Ok(dir)
    }
}

// TODO: add a proper owned version here that moves various fields
impl From<crate::Directory> for Directory {
    fn from(value: crate::Directory) -> Self {
        (&value).into()
    }
}

impl From<&crate::Directory> for Directory {
    fn from(directory: &crate::Directory) -> Directory {
        let mut directories = vec![];
        let mut files = vec![];
        let mut symlinks = vec![];

        for (name, node) in directory.nodes() {
            match node {
                crate::Node::File(n) => files.push(FileNode {
                    name: name.clone(),
                    digest: n.digest().to_owned().into(),
                    size: n.size(),
                    executable: n.executable(),
                }),
                crate::Node::Directory(n) => directories.push(DirectoryNode {
                    name: name.clone(),
                    digest: n.digest().to_owned().into(),
                    size: n.size(),
                }),
                crate::Node::Symlink(n) => {
                    symlinks.push(SymlinkNode {
                        name: name.clone(),
                        target: n.target().to_owned(),
                    });
                }
            }
        }
        Directory {
            directories,
            files,
            symlinks,
        }
    }
}

impl Node {
    /// Converts a proto [Node] to a [crate::Node], and splits off the name.
    pub fn into_name_and_node(self) -> Result<(bytes::Bytes, crate::Node), DirectoryError> {
        match self.node.ok_or_else(|| DirectoryError::NoNodeSet)? {
            node::Node::Directory(n) => {
                let digest = B3Digest::try_from(n.digest)
                    .map_err(|e| DirectoryError::InvalidNode(n.name.to_owned(), e.into()))?;

                let node = crate::Node::Directory(
                    crate::DirectoryNode::new(digest, n.size)
                        .map_err(|e| DirectoryError::InvalidNode(n.name.to_owned(), e))?,
                );

                Ok((n.name, node))
            }
            node::Node::File(n) => {
                let digest = B3Digest::try_from(n.digest)
                    .map_err(|e| DirectoryError::InvalidNode(n.name.to_owned(), e.into()))?;

                let node = crate::Node::File(crate::FileNode::new(digest, n.size, n.executable));

                Ok((n.name, node))
            }

            node::Node::Symlink(n) => {
                let node = crate::Node::Symlink(
                    crate::SymlinkNode::new(n.target)
                        .map_err(|e| DirectoryError::InvalidNode(n.name.to_owned(), e))?,
                );

                Ok((n.name, node))
            }
        }
    }

    /// Construsts a [Node] from a name and [crate::Node].
    pub fn from_name_and_node(name: bytes::Bytes, n: crate::Node) -> Self {
        // TODO: make these pub(crate) so we can avoid cloning?
        match n {
            crate::Node::Directory(directory_node) => Self {
                node: Some(node::Node::Directory(DirectoryNode {
                    name,
                    digest: directory_node.digest().to_owned().into(),
                    size: directory_node.size(),
                })),
            },
            crate::Node::File(file_node) => Self {
                node: Some(node::Node::File(FileNode {
                    name,
                    digest: file_node.digest().to_owned().into(),
                    size: file_node.size(),
                    executable: file_node.executable(),
                })),
            },
            crate::Node::Symlink(symlink_node) => Self {
                node: Some(node::Node::Symlink(SymlinkNode {
                    name,
                    target: symlink_node.target().to_owned(),
                })),
            },
        }
    }
}

impl StatBlobResponse {
    /// Validates a StatBlobResponse. All chunks must have valid blake3 digests.
    /// It is allowed to send an empty list, if no more granular chunking is
    /// available.
    pub fn validate(&self) -> Result<(), ValidateStatBlobResponseError> {
        for (i, chunk) in self.chunks.iter().enumerate() {
            if chunk.digest.len() != blake3::KEY_LEN {
                return Err(ValidateStatBlobResponseError::InvalidDigestLen(
                    chunk.digest.len(),
                    i,
                ));
            }
        }
        Ok(())
    }
}
