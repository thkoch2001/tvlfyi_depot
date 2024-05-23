use std::collections::HashSet;

use crate::{proto::Directory, B3Digest};

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("{0}")]
    InvalidOrder(String),
    #[error("The graph is incomplete, or there are disconnected nodes")]
    DisconnectedNodes,
}

pub trait OrderValidator {
    /// Check if this directory would be in order
    /// Must be called before `add_directory`, and
    /// should be called before parsing the record if possible
    fn digest_allowed(&mut self, digest: &B3Digest) -> bool;
    /// Update the order validator's state with the directory
    fn add_directory(&mut self, directory: &Directory) -> Result<(), Error>;
    /// Get the root directory. Returns None if no directories have been added yet.
    /// Returns an Error if not the entire graph is connected to the same root.
    /// May panic if the directory contains invalid data like Blake3 digests
    fn root(&self) -> Result<Option<B3Digest>, Error>;
}

#[derive(Default)]
pub struct RootToLeavesValidator {
    /// Only used to remember the root node, not for validation
    root: Option<B3Digest>,

    /// The Directory digests we're expecting to receive
    expected_digests: HashSet<B3Digest>,
}

impl OrderValidator for RootToLeavesValidator {
    fn digest_allowed(&mut self, digest: &B3Digest) -> bool {
        self.expected_digests.is_empty() // we don't know the root node; allow any
            || self.expected_digests.contains(digest)
    }

    fn add_directory(&mut self, directory: &Directory) -> Result<(), Error> {
        if self.root.is_none() {
            let root_digest = directory.digest();
            self.root = Some(root_digest.clone());
            self.expected_digests.insert(root_digest);
        }

        for subdir in &directory.directories {
            // Allow the children to appear next
            let subdir_digest = subdir.digest.clone().try_into().unwrap();
            self.expected_digests.insert(subdir_digest);
        }
        Ok(())
    }

    fn root(&self) -> Result<Option<B3Digest>, Error> {
        Ok(self.root.clone())
    }
}

#[derive(Default)]
pub struct LeavesToRootValidator {
    /// This is empty in the beginning, and gets filled as leaves and intermediates are
    /// inserted
    allowed_references: HashSet<B3Digest>,

    /// Directories which have not been referenced yet
    /// In the end there should be only one, which is the root
    dangling_roots: HashSet<B3Digest>,
}

impl OrderValidator for LeavesToRootValidator {
    fn digest_allowed(&mut self, _digest: &B3Digest) -> bool {
        // There is no way to know what will be the next directory
        true
    }

    fn add_directory(&mut self, directory: &Directory) -> Result<(), Error> {
        let digest = directory.digest();

        for subdir in &directory.directories {
            let subdir_digest = subdir.digest.clone().try_into().unwrap(); // this has been validated in validate_directory()
            if !self.allowed_references.contains(&subdir_digest) {
                return Err(Error::InvalidOrder(format!(
                    "unexpected directory reference: {} -> {}",
                    data_encoding::HEXLOWER.encode(digest.as_slice()),
                    data_encoding::HEXLOWER.encode(subdir_digest.as_slice())
                )));
            }
            self.dangling_roots.remove(&subdir_digest);
        }

        self.allowed_references.insert(digest.clone());
        self.dangling_roots.insert(digest);

        Ok(())
    }

    fn root(&self) -> Result<Option<B3Digest>, Error> {
        let mut iter = self.dangling_roots.iter();
        let root = iter.next();
        match iter.next() {
            Some(_) => Err(Error::DisconnectedNodes),
            None => Ok(root.cloned()),
        }
    }
}
