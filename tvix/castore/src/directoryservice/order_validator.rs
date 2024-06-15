use std::collections::HashSet;
use tracing::warn;

use crate::{proto::Directory, B3Digest};

pub trait OrderValidator {
    /// Update the order validator's state with the directory
    /// Returns if the insert was successful
    fn add_directory(&mut self, directory: &Directory) -> bool;
}

#[derive(Default)]
pub struct RootToLeavesValidator {
    /// Only used to remember the root node, not for validation
    expected_digests: HashSet<B3Digest>,
}

impl RootToLeavesValidator {
    pub fn new_with_root_digest(root_digest: B3Digest) -> Self {
        let mut this = Self::default();
        this.expected_digests.insert(root_digest);
        this
    }

    pub fn digest_allowed(&self, digest: &B3Digest) -> bool {
        self.expected_digests.is_empty() // we don't know the root node; allow any
            || self.expected_digests.contains(digest)
    }

    pub fn add_directory_unchecked(&mut self, directory: &Directory) {
        if self.expected_digests.is_empty() {
            self.expected_digests.insert(directory.digest());
        }

        for subdir in &directory.directories {
            // Allow the children to appear next
            let subdir_digest = subdir.digest.clone().try_into().unwrap();
            self.expected_digests.insert(subdir_digest);
        }
    }
}

impl OrderValidator for RootToLeavesValidator {
    fn add_directory(&mut self, directory: &Directory) -> bool {
        if !self.digest_allowed(&directory.digest()) {
            return false;
        }
        self.add_directory_unchecked(directory);
        true
    }
}

#[derive(Default)]
pub struct LeavesToRootValidator {
    /// This is empty in the beginning, and gets filled as leaves and intermediates are
    /// inserted
    allowed_references: HashSet<B3Digest>,
}

impl OrderValidator for LeavesToRootValidator {
    fn add_directory(&mut self, directory: &Directory) -> bool {
        let digest = directory.digest();

        for subdir in &directory.directories {
            let subdir_digest = subdir.digest.clone().try_into().unwrap(); // this has been validated in validate_directory()
            if !self.allowed_references.contains(&subdir_digest) {
                warn!(
                    "unexpected directory reference: {} -> {}",
                    data_encoding::HEXLOWER.encode(digest.as_slice()),
                    data_encoding::HEXLOWER.encode(subdir_digest.as_slice())
                );
                return false;
            }
        }

        self.allowed_references.insert(digest.clone());

        true
    }
}
