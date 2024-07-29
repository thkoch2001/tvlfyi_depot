#![allow(non_snake_case)]
// https://github.com/hyperium/tonic/issues/1056
use bstr::ByteSlice;
use std::{collections::HashSet, iter::Peekable, str};

use prost::Message;

mod grpc_blobservice_wrapper;
mod grpc_directoryservice_wrapper;

pub use grpc_blobservice_wrapper::GRPCBlobServiceWrapper;
pub use grpc_directoryservice_wrapper::GRPCDirectoryServiceWrapper;

use crate::{B3Digest, B3_LEN, ValidateNodeError, ValidateDirectoryError};

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

impl Node {
    /// Ensures the node has a valid enum kind (is Some), and passes its
    /// per-enum validation.
    /// The inner root node is returned for easier consumption.
    pub fn validate(&self) -> Result<crate::directoryservice::Node, ValidateNodeError> {
        if let Some(node) = self.node.as_ref() {
            node.validate()
        } else {
            Err(ValidateNodeError::NoNodeSet)
        }
    }
}

impl TryFrom<node::Node> for crate::directoryservice::Node {
    type Error = ValidateNodeError;

    fn try_from(node: node::Node) -> Result<crate::directoryservice::Node, ValidateNodeError> {
        match node {
            // for a directory root node, ensure the digest has the appropriate size.
            node::Node::Directory(directory_node) => {
                crate::directoryservice::Node::Directory(directory_node.try_into()?)
            }
            // for a file root node, ensure the digest has the appropriate size.
            node::Node::File(file_node) => {
                crate::directoryservice::Node::File(file_node.try_into()?)
            }
            // ensure the symlink target is not empty and doesn't contain null bytes.
            node::Node::Symlink(symlink_node) => {
                crate::directoryservice::Node::Symlink(symlink_node.try_into()?)
            }
        }
    }
}

/// Accepts a name, and a mutable reference to the previous name.
/// If the passed name is larger than the previous one, the reference is updated.
/// If it's not, an error is returned.
fn update_if_lt_prev<'n>(
    prev_name: &mut &'n [u8],
    name: &'n [u8],
) -> Result<(), ValidateDirectoryError> {
    if *name < **prev_name {
        return Err(ValidateDirectoryError::WrongSorting(name.to_vec()));
    }
    *prev_name = name;
    Ok(())
}

/// Inserts the given name into a HashSet if it's not already in there.
/// If it is, an error is returned.
fn insert_once<'n>(
    seen_names: &mut HashSet<&'n [u8]>,
    name: &'n [u8],
) -> Result<(), ValidateDirectoryError> {
    if seen_names.get(name).is_some() {
        return Err(ValidateDirectoryError::DuplicateName(name.to_vec()));
    }
    seen_names.insert(name);
    Ok(())
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

    /// validate checks the directory for invalid data, such as:
    /// - violations of name restrictions
    /// - invalid digest lengths
    /// - not properly sorted lists
    /// - duplicate names in the three lists
    pub fn validate(&self) -> Result<(), ValidateDirectoryError> {
        let mut seen_names: HashSet<&[u8]> = HashSet::new();

        let mut last_directory_name: &[u8] = b"";
        let mut last_file_name: &[u8] = b"";
        let mut last_symlink_name: &[u8] = b"";

        // check directories
        for directory_node in &self.directories {
            node::Node::Directory(directory_node.clone())
                .validate()
                .map_err(|e| {
                    ValidateDirectoryError::InvalidNode(directory_node.name.to_vec(), e)
                })?;

            update_if_lt_prev(&mut last_directory_name, &directory_node.name)?;
            insert_once(&mut seen_names, &directory_node.name)?;
        }

        // check files
        for file_node in &self.files {
            node::Node::File(file_node.clone())
                .validate()
                .map_err(|e| ValidateDirectoryError::InvalidNode(file_node.name.to_vec(), e))?;

            update_if_lt_prev(&mut last_file_name, &file_node.name)?;
            insert_once(&mut seen_names, &file_node.name)?;
        }

        // check symlinks
        for symlink_node in &self.symlinks {
            node::Node::Symlink(symlink_node.clone())
                .validate()
                .map_err(|e| ValidateDirectoryError::InvalidNode(symlink_node.name.to_vec(), e))?;

            update_if_lt_prev(&mut last_symlink_name, &symlink_node.name)?;
            insert_once(&mut seen_names, &symlink_node.name)?;
        }

        self.size_checked()
            .ok_or(ValidateDirectoryError::SizeOverflow)?;

        Ok(())
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

