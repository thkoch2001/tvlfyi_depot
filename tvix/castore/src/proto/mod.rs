use std::str;

use prost::Message;

mod grpc_blobservice_wrapper;
mod grpc_directoryservice_wrapper;

pub use grpc_blobservice_wrapper::GRPCBlobServiceWrapper;
pub use grpc_directoryservice_wrapper::GRPCDirectoryServiceWrapper;

use crate::directoryservice::NamedNode;
use crate::{B3Digest, ValidateDirectoryError};

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

impl TryFrom<Directory> for crate::directoryservice::Directory {
    type Error = ValidateDirectoryError;

    fn try_from(
        directory: Directory,
    ) -> Result<crate::directoryservice::Directory, ValidateDirectoryError> {
        todo!()
    }
}

impl From<&crate::directoryservice::Node> for node::Node {
    fn from(node: &crate::directoryservice::Node) -> node::Node {
        match node {
            crate::directoryservice::Node::Directory(n) => node::Node::Directory(n.into()),
            crate::directoryservice::Node::File(n) => node::Node::File(n.into()),
            crate::directoryservice::Node::Symlink(n) => node::Node::Symlink(n.into()),
        }
    }
}

impl From<&crate::directoryservice::DirectoryNode> for DirectoryNode {
    fn from(node: &crate::directoryservice::DirectoryNode) -> DirectoryNode {
        DirectoryNode {
            digest: node.digest().clone().into(),
            size: node.size(),
            name: node.get_name().clone(),
        }
    }
}

impl From<&crate::directoryservice::FileNode> for FileNode {
    fn from(node: &crate::directoryservice::FileNode) -> FileNode {
        FileNode {
            digest: node.digest().clone().into(),
            size: node.size(),
            name: node.get_name().clone(),
            executable: node.executable(),
        }
    }
}

impl From<&crate::directoryservice::SymlinkNode> for SymlinkNode {
    fn from(node: &crate::directoryservice::SymlinkNode) -> SymlinkNode {
        SymlinkNode {
            name: node.get_name().clone(),
            target: node.target().clone(),
        }
    }
}

impl From<crate::directoryservice::Directory> for Directory {
    fn from(directory: crate::directoryservice::Directory) -> Directory {
        (&directory).into()
    }
}

impl From<&crate::directoryservice::Directory> for Directory {
    fn from(directory: &crate::directoryservice::Directory) -> Directory {
        let mut directories = vec![];
        let mut files = vec![];
        let mut symlinks = vec![];
        for node in directory.nodes() {
            match node {
                crate::directoryservice::Node::File(n) => {
                    files.push(n.into());
                }
                crate::directoryservice::Node::Directory(n) => {
                    directories.push(n.into());
                }
                crate::directoryservice::Node::Symlink(n) => {
                    symlinks.push(n.into());
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
