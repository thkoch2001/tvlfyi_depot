use std::collections::{HashMap, HashSet};

use bstr::ByteSlice;

use tracing::{instrument, trace};

use crate::{
    proto::{self, Directory},
    B3Digest, Error,
};

/// This can be used to validate a Directory closure (graph of connected
/// Directories), and their insertion order.
///
/// Directories need to be inserted (via `add`), in an order from the leaves to
/// the root. During insertion, We validate as much as we can at that time:
///
///  - individual validation of Directory messages
///  - validation of insertion order (no upload of not-yet-known Directories)
///  - validation of size fields of referred Directories
///
/// Internally it keeps all received Directories (and their sizes) in a HashMap,
/// keyed by digest.
///
/// Once all Directories have been inserted, a drain() function can be called to
/// get a (deduplicated and) validated list of directories, in
/// from-leaves-to-root order (to be stored somewhere).
/// While assembling that list, a check for graph connectivity is performed too,
/// to ensure there's no disconnected graphs, and only one root.
#[derive(Default)]
pub struct DirectoryClosureValidator {
    directories_and_sizes: HashMap<B3Digest, (Directory, u64)>,

    /// Keeps track of the last-inserted directory digest. Used to start the
    /// connectivity check.
    last_directory_digest: Option<B3Digest>,
}

impl DirectoryClosureValidator {
    /// Insert a new Directory into the closure.
    /// Perform individual Directory validation, validation of insertion order
    // and size fields.
    #[instrument(level = "trace", skip_all, fields(directory.digest=%directory.digest()), err)]
    pub fn add(&mut self, directory: proto::Directory) -> Result<(), Error> {
        let digest = directory.digest();

        if self.directories_and_sizes.contains_key(&digest) {
            trace!("already seen, skipping");
            return Ok(());
        }

        // Do some general validation
        directory
            .validate()
            .map_err(|e| Error::InvalidRequest(e.to_string()))?;

        // Ensure the directory only refers to directories which we already accepted.
        for dir in &directory.directories {
            let dir_dgst = B3Digest::try_from(dir.digest.to_owned()).unwrap(); // validated

            // Ensure the digest has already been seen
            let (_, recorded_dir_size) =
                self.directories_and_sizes.get(&dir_dgst).ok_or_else(|| {
                    Error::InvalidRequest(format!(
                        "'{}' refers to unseen child dir: {}",
                        dir.name.as_bstr(),
                        dir_dgst
                    ))
                })?;

            // Ensure the size specified in the child node matches our records.
            if dir.size != *recorded_dir_size {
                return Err(Error::InvalidRequest(format!(
                    "'{}' has wrong size, specified {}, recorded {}",
                    dir.name.as_bstr(),
                    dir.size,
                    recorded_dir_size
                )));
            }
        }

        trace!("inserting");
        let directory_size = directory.size();
        self.directories_and_sizes
            .insert(digest.clone(), (directory, directory_size));
        self.last_directory_digest = Some(digest);

        Ok(())
    }

    /// Ensure that all inserted Directories are connected, then return a
    /// (deduplicated) and validated list of directories, in from-leaves-to-root
    /// order.
    /// In case no elements have been inserted, returns an empty list.
    #[instrument(level = "trace", skip_all, err)]
    pub(crate) fn drain(mut self) -> Result<Vec<Directory>, Error> {
        if self.last_directory_digest.is_none() {
            return Ok(vec![]);
        }
        let root_digest = self.last_directory_digest.unwrap();

        // recursively walk all directories reachable from there,
        // ensure we visited all nodes that were uploaded.
        // If not, we might have received multiple disconnected graphs.

        // The list of directories we know we need to visit.
        // Once we're finished working, and (there's no errors), this in reversed order will
        // be a valid insertion order, and directories_and_sizes will be empty.
        let mut worklist = Vec::from([self
            .directories_and_sizes
            .remove(&root_digest)
            .expect("root digest not found")
            .0]);
        // The set of digests that are in the worklist.
        let mut worklist_digests = HashSet::new();

        // This loop moves gradually to the end of worklist, while it's being
        // extended.
        // The loop stops once it reaches the end.
        let mut i = 0;
        while let Some(directory) = worklist.get(i).map(|d| d.to_owned()) {
            // lookup all child directories and put them in the back of the worklist,
            // if they're not already there.
            for child_dir in &directory.directories {
                let child_digest = B3Digest::try_from(child_dir.digest.to_owned()).unwrap(); // validated

                // in case the digest is neither already visited nor already on the worklist,
                // enqueue it.
                // We don't need to check for the hash we're currently
                // visiting, as we can't self-reference.
                if !worklist_digests.contains(&child_digest) {
                    worklist.push(
                        self.directories_and_sizes
                            .remove(&child_digest)
                            .expect("child digest not found")
                            .0,
                    );
                    worklist_digests.insert(child_digest);
                }
            }

            i += 1;
        }

        // check directories_and_sizes is empty.
        if !self.directories_and_sizes.is_empty() {
            if cfg!(debug_assertions) {
                return Err(Error::InvalidRequest(format!(
                    "found {} disconnected nodes: {:?}",
                    self.directories_and_sizes.len(),
                    self.directories_and_sizes
                )));
            } else {
                return Err(Error::InvalidRequest(format!(
                    "found {} disconnected nodes",
                    self.directories_and_sizes.len()
                )));
            }
        }

        // Reverse to have correct insertion order.
        worklist.reverse();

        Ok(worklist)
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        fixtures::{DIRECTORY_A, DIRECTORY_B, DIRECTORY_C},
        proto::{self, Directory},
    };
    use lazy_static::lazy_static;
    use test_case::test_case;

    lazy_static! {
        pub static ref BROKEN_DIRECTORY : Directory = Directory {
            symlinks: vec![proto::SymlinkNode {
                name: "".into(), // invalid name!
                target: "doesntmatter".into(),
            }],
            ..Default::default()
        };

        pub static ref BROKEN_PARENT_DIRECTORY: Directory = Directory {
            directories: vec![proto::DirectoryNode {
                name: "foo".into(),
                digest: DIRECTORY_A.digest().into(),
                size: DIRECTORY_A.size() + 42, // wrong!
            }],
            ..Default::default()
        };
    }

    use super::DirectoryClosureValidator;

    /// Uploading an empty directory should succeed.
    #[test_case(vec![&DIRECTORY_A], false, Some(vec![&DIRECTORY_A]); "empty directory")]
    /// Uploading A, then B (referring to A) should succeed.
    #[test_case(vec![&DIRECTORY_A, &DIRECTORY_B], false, Some(vec![&DIRECTORY_A, &DIRECTORY_B]); "simple closure")]
    /// Uploading A, then A, then C (referring to A twice) should succeed.
    /// We pretend to be a dumb client not deduping directories.
    #[test_case(vec![&DIRECTORY_A, &DIRECTORY_A, &DIRECTORY_C], false, Some(vec![&DIRECTORY_A, &DIRECTORY_C]); "same child")]
    /// Uploading A, then C (referring to A twice) should succeed.
    #[test_case(vec![&DIRECTORY_A, &DIRECTORY_C], false, Some(vec![&DIRECTORY_A, &DIRECTORY_C]); "same child dedup")]
    /// Uploading A, then C (referring to A twice), then B (itself referring to A) should fail during close,
    /// as B itself would be left unconnected.
    #[test_case(vec![&DIRECTORY_A, &DIRECTORY_C, &DIRECTORY_B], false, None; "unconnected node")]
    /// Uploading B (referring to A) should fail immediately, because A was never uploaded.
    #[test_case(vec![&DIRECTORY_B], true, None; "dangling pointer")]
    /// Uploading a directory failing validation should fail immediately.
    #[test_case(vec![&BROKEN_DIRECTORY], true, None; "failing validation")]
    /// Uploading a directory which refers to another Directory with a wrong size should fail.
    #[test_case(vec![&DIRECTORY_A, &BROKEN_PARENT_DIRECTORY], true, None; "wrong size in parent")]
    fn test_uploads(
        directories_to_upload: Vec<&Directory>,
        exp_fail_upload_last: bool,
        exp_drain: Option<Vec<&Directory>>, // Some(_) if drain successful, None if not.
    ) {
        let mut dcv = DirectoryClosureValidator::default();
        let len_directories_to_upload = directories_to_upload.len();

        for (i, d) in directories_to_upload.iter().enumerate() {
            let resp = dcv.add((*d).clone());
            if i == len_directories_to_upload - 1 && exp_fail_upload_last {
                assert!(resp.is_err(), "expect last put to fail");

                // We don't really care anymore what drain() would return, as
                // the add() failed.
                // TODO: burn a fuse?
                return;
            } else {
                assert!(resp.is_ok(), "expect put to succeed");
            }
        }

        // everything was uploaded successfully. Test drain().
        let resp = dcv.drain();

        match exp_drain {
            Some(exp_drain) => {
                assert_eq!(
                    Vec::from_iter(exp_drain.into_iter().map(|e| e.to_owned())),
                    resp.expect("drain should succeed")
                );
            }
            None => {
                resp.expect_err("drain should fail");
            }
        }
    }
}
