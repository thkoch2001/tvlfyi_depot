use std::{collections::HashMap, rc::Rc};

use crate::{proto, B3Digest};

#[derive(Clone, Debug)]
pub enum InodeData {
    Regular(B3Digest, u32, bool), // digest, size, executable
    Symlink(String),              // target

    // This encodes data about a directory.
    // The 2nd part of the tuple being None means that data hasn't been fetched yet.
    // This happens when visiting a Directory, which itself contains a child directory,
    // for which we reserve an Inode (so the lookup for it can return something),
    // but we didn't end up inserting its listing yet.
    Directory(B3Digest, Option<Vec<(u64, proto::node::Node)>>), // (B3Digest, Option<[(inode, proto::node::Node)]>)
}
// type DirectoryListing = BTreeMap<OsString, (u64, fuser::FileType)>;

/// converts a proto::Directory to a InodeData::Directory.
/// The inodes for each child are 0, because we can't know them yet.
impl From<proto::Directory> for InodeData {
    fn from(value: proto::Directory) -> Self {
        let digest = value.digest();

        let children: Vec<(u64, proto::node::Node)> = value.nodes().map(|node| (0, node)).collect();

        InodeData::Directory(digest, Some(children))
    }
}

impl From<&InodeData> for fuser::FileType {
    fn from(val: &InodeData) -> Self {
        match val {
            InodeData::Regular(..) => fuser::FileType::RegularFile,
            InodeData::Symlink(_) => fuser::FileType::Symlink,
            InodeData::Directory(..) => fuser::FileType::Directory,
        }
    }
}

/// InodeTracker keeps track of inodes and stores some data alongside them.
pub struct InodeTracker {
    data: HashMap<u64, Rc<InodeData>>,

    // lookup table for blobs by their B3Digest
    blob_digest_to_inode: HashMap<B3Digest, u64>,

    // lookup table for symlinks by their target
    symlink_target_to_inode: HashMap<String, u64>,

    // lookup table for directories by their B3Digest.
    // Note the corresponding directory may not be present in data yet.
    directory_digest_to_inode: HashMap<B3Digest, u64>,

    // the next inode to allocate
    next_inode: u64,
}

impl Default for InodeTracker {
    fn default() -> Self {
        Self {
            data: Default::default(),

            blob_digest_to_inode: Default::default(),
            symlink_target_to_inode: Default::default(),
            directory_digest_to_inode: Default::default(),

            next_inode: 1,
        }
    }
}

impl InodeTracker {
    // Retrieves data for a given inode, if it exists.
    pub fn get(&self, ino: u64) -> Option<Rc<InodeData>> {
        self.data.get(&ino).cloned()
    }

    // Stores data and returns the inode for it.
    // In case an inode has already been allocated for the same data, that inode
    // is returned, otherwise a new one is allocated.
    // In case data is a [InodeData::Directory], inodes for all items are looked
    // up
    pub fn put(&mut self, data: InodeData) -> u64 {
        match data {
            InodeData::Regular(ref digest, _, _) => {
                match self.blob_digest_to_inode.get(digest) {
                    Some(ino) => {
                        // We already have it, return the inode.
                        *ino
                    }
                    None => self.insert_and_increment(data),
                }
            }
            InodeData::Symlink(ref target) => {
                match self.symlink_target_to_inode.get(target) {
                    Some(ino) => {
                        // We already have it, return the inode.
                        *ino
                    }
                    None => self.insert_and_increment(data),
                }
            }
            InodeData::Directory(digest, children) => {
                if children.is_none() {
                    panic!("you may not put None as children during put");
                }

                match self.directory_digest_to_inode.get(&digest) {
                    Some(ino) => {
                        // check self.data. if its children are still None,
                        // override the data with the new populated children.
                        match *self.data.get(ino).unwrap().clone() {
                            InodeData::Regular(..) | InodeData::Symlink(_) => {
                                panic!("wrong type"); // this can't happen
                            }
                            InodeData::Directory(_, None) => {
                                self.data
                                    .insert(*ino, Rc::new(InodeData::Directory(digest, children)));
                            }
                            InodeData::Directory(_, Some(_)) => {}
                        };

                        *ino
                    }
                    None => {
                        let children = children
                            .unwrap()
                            .into_iter()
                            .map(|child| {
                                // look up / allocate inodes for the child
                                let child_inode = match child.1 {
                                    // For files, we peek at at our lookup table and allocate/insert.
                                    proto::node::Node::File(ref child_file_node) => {
                                        let child_file_digest =
                                            B3Digest::from_vec(child_file_node.digest.clone())
                                                .unwrap();
                                        match self.blob_digest_to_inode.get(&child_file_digest) {
                                            Some(child_ino) => *child_ino,
                                            None => self.insert_and_increment(InodeData::Regular(
                                                child_file_digest,
                                                child_file_node.size,
                                                child_file_node.executable,
                                            )),
                                        }
                                    }

                                    // For symlinks, we peek at at our lookup table and allocate/insert too.
                                    proto::node::Node::Symlink(ref child_symlink_node) => {
                                        match self
                                            .symlink_target_to_inode
                                            .get(&child_symlink_node.target)
                                        {
                                            Some(child_ino) => *child_ino,
                                            None => self.insert_and_increment(InodeData::Symlink(
                                                child_symlink_node.target.clone(),
                                            )),
                                        }
                                    }

                                    // Directories are a bit messy. We can look at the lookup table,
                                    // but we can't populate/insert the child data in case it doesn't exist yet,
                                    // as that'd mean we'd need to have it already.
                                    proto::node::Node::Directory(ref child_directory_node) => {
                                        let child_directory_digest =
                                            B3Digest::from_vec(child_directory_node.digest.clone())
                                                .unwrap();
                                        match self
                                            .directory_digest_to_inode
                                            .get(&child_directory_digest)
                                        {
                                            Some(child_ino) => *child_ino,
                                            None => {
                                                // insert the (sparse) [InodeData::Directory]
                                                self.insert_and_increment(InodeData::Directory(
                                                    child_directory_digest,
                                                    None,
                                                ))
                                            }
                                        }
                                    }
                                };
                                (child_inode, child.1)
                            })
                            .collect();

                        self.insert_and_increment(InodeData::Directory(digest, Some(children)))
                    }
                }
            }
        }
    }

    // Inserts the data and returns the inode it was stored at, while
    // incrementing next_inode.
    fn insert_and_increment(&mut self, data: InodeData) -> u64 {
        let ino = self.next_inode;
        // insert into lookup tables
        match data {
            InodeData::Regular(ref digest, _, _) => {
                self.blob_digest_to_inode.insert(digest.clone(), ino);
            }
            InodeData::Symlink(ref target) => {
                self.symlink_target_to_inode.insert(target.to_string(), ino);
            }
            InodeData::Directory(ref digest, _) => {
                self.directory_digest_to_inode.insert(digest.clone(), ino);
            }
        }
        // Insert data
        self.data.insert(ino, Rc::new(data));

        // increment inode counter and return old inode.
        self.next_inode += 1;
        ino
    }
}

#[cfg(test)]
mod tests {
    use crate::proto;
    use crate::tests::fixtures;

    use super::InodeData;
    use super::InodeTracker;

    /// Getting something non-existent should be none
    #[test]
    fn get_nonexistent() {
        let inode_tracker = InodeTracker::default();
        assert!(inode_tracker.get(1).is_none());
    }

    /// Put of a regular file should allocate a uid, which should be the same when inserting again.
    #[test]
    fn put_regular() {
        let mut inode_tracker = InodeTracker::default();
        let f = InodeData::Regular(
            fixtures::BLOB_A_DIGEST.clone(),
            fixtures::BLOB_A.len() as u32,
            false,
        );

        // put it in
        let ino = inode_tracker.put(f.clone());

        // a get should return the right data
        let data = inode_tracker.get(ino).expect("must be some");
        match *data {
            InodeData::Regular(ref digest, _, _) => {
                assert_eq!(&fixtures::BLOB_A_DIGEST.clone(), digest);
            }
            InodeData::Symlink(_) | InodeData::Directory(..) => panic!("wrong type"),
        }

        // another put should return the same ino
        assert_eq!(ino, inode_tracker.put(f));

        // inserting another file should return a different ino
        assert_ne!(
            ino,
            inode_tracker.put(InodeData::Regular(
                fixtures::BLOB_B_DIGEST.clone(),
                fixtures::BLOB_B.len() as u32,
                false,
            ))
        );
    }

    // Put of a symlink should allocate a uid, which should be the same when inserting again
    #[test]
    fn put_symlink() {
        let mut inode_tracker = InodeTracker::default();
        let f = InodeData::Symlink("target".to_string());

        // put it in
        let ino = inode_tracker.put(f.clone());

        // a get should return the right data
        let data = inode_tracker.get(ino).expect("must be some");
        match *data {
            InodeData::Symlink(ref target) => {
                assert_eq!("target", target);
            }
            InodeData::Regular(..) | InodeData::Directory(..) => panic!("wrong type"),
        }

        // another put should return the same ino
        assert_eq!(ino, inode_tracker.put(f));

        // inserting another file should return a different ino
        assert_ne!(
            ino,
            inode_tracker.put(InodeData::Symlink("target2".to_string()))
        );
    }

    /// Put a directory into the inode tracker, which refers to a file not seen yet.
    #[test]
    fn put_directory_leaf() {
        let mut inode_tracker = InodeTracker::default();

        // this is a directory with a single item, a ".keep" file pointing to a 0 bytes blob.
        let dir: InodeData = fixtures::DIRECTORY_WITH_KEEP.clone().into();

        // put it in
        let ino = inode_tracker.put(dir.clone());

        // a get should return the right data
        let data = inode_tracker.get(ino).expect("must be some");
        match *data {
            InodeData::Directory(ref directory_digest, ref children) => {
                // ensure the directory digest matches
                assert_eq!(&fixtures::DIRECTORY_WITH_KEEP.digest(), directory_digest);

                let children = children.as_ref().expect("must be some");

                // ensure the child is populated, with a different inode than
                // the parent, and the data matches expectations.
                assert_eq!(1, children.len());
                let (child_node_inode, child_node) = children.first().unwrap();
                assert_ne!(ino, *child_node_inode);
                assert_eq!(
                    &proto::node::Node::File(
                        fixtures::DIRECTORY_WITH_KEEP.files.first().unwrap().clone()
                    ),
                    child_node
                );

                // ensure looking up that inode directly also returns the data
                let child_data = inode_tracker.get(*child_node_inode).expect("must exist");
                match *child_data {
                    InodeData::Regular(ref digest, size, executable) => {
                        assert_eq!(&fixtures::EMPTY_BLOB_DIGEST.clone(), digest);
                        assert_eq!(0, size);
                        assert_eq!(false, executable);
                    }
                    InodeData::Symlink(_) | InodeData::Directory(..) => panic!("wrong type"),
                }
            }
            InodeData::Symlink(_) | InodeData::Regular(..) => panic!("wrong type"),
        }
    }

    /// Put a directory into the inode tracker, referring to files, directories
    /// and symlinks not seen yet.
    #[test]
    fn put_directory_complicated() {
        let mut inode_tracker = InodeTracker::default();

        // this is a directory with a single item, a ".keep" file pointing to a 0 bytes blob.
        let dir_complicated: InodeData = fixtures::DIRECTORY_COMPLICATED.clone().into();

        // put it in
        let ino = inode_tracker.put(dir_complicated.clone());

        // a get should return the right data
        let data = inode_tracker.get(ino).expect("must be some");

        let child_dir_ino = match *data {
            InodeData::Directory(ref directory_digest, ref children) => {
                // assert the directory digest matches
                assert_eq!(&fixtures::DIRECTORY_COMPLICATED.digest(), directory_digest);

                let children = children.as_ref().expect("must be some");

                // ensure there's three children, all with different inodes
                assert_eq!(3, children.len());
                let mut seen_inodes = Vec::from([ino]);

                // check the first child (.keep)
                {
                    let (child_ino, child_node) = &children[0];
                    assert!(!seen_inodes.contains(&child_ino));
                    assert_eq!(
                        &proto::node::Node::File(fixtures::DIRECTORY_COMPLICATED.files[0].clone()),
                        child_node
                    );
                    seen_inodes.push(*child_ino);
                }

                // check the second child (aa)
                {
                    let (child_ino, child_node) = &children[1];
                    assert!(!seen_inodes.contains(&child_ino));
                    assert_eq!(
                        &proto::node::Node::Symlink(
                            fixtures::DIRECTORY_COMPLICATED.symlinks[0].clone()
                        ),
                        child_node
                    );
                    seen_inodes.push(*child_ino);
                }

                // check the third child (keep)
                {
                    let (child_ino, child_node) = &children[2];
                    assert!(!seen_inodes.contains(&child_ino));
                    assert_eq!(
                        &proto::node::Node::Directory(
                            fixtures::DIRECTORY_COMPLICATED.directories[0].clone()
                        ),
                        child_node
                    );
                    seen_inodes.push(*child_ino);

                    // return the child_ino
                    *child_ino
                }
            }
            InodeData::Regular(..) | InodeData::Symlink(_) => panic!("wrong type"),
        };

        // get of the inode for child_ino
        let child_dir_data = inode_tracker.get(child_dir_ino).expect("must be some");
        // it should be a InodeData::Directory with the right digest, and None for children
        match *child_dir_data {
            InodeData::Directory(ref digest, ref children) => {
                assert_eq!(&fixtures::DIRECTORY_WITH_KEEP.digest(), digest);
                assert_eq!(&None, children);
            }
            InodeData::Regular(..) | InodeData::Symlink(_) => panic!("wrong type"),
        }

        // put DIRECTORY_WITH_KEEP, which should return the same ino as child_dir_ino,
        // but update the sparse object at the same time.
        let child_dir_ino2 = inode_tracker.put(fixtures::DIRECTORY_WITH_KEEP.clone().into());
        assert_eq!(child_dir_ino, child_dir_ino2);

        // get of the inode for child_ino
        let data = inode_tracker.get(child_dir_ino).expect("must be some");
        // dbg!(&child_dir_data);

        // it should be a InodeData::Directory with the right digest, and children now populated!

        match *data {
            InodeData::Directory(ref directory_digest, ref children) => {
                // ensure the directory digest matches
                assert_eq!(&fixtures::DIRECTORY_WITH_KEEP.digest(), directory_digest);

                let children = children.as_ref().expect("must be some");

                // ensure the child is populated, with a different inode than
                // the parent, and the data matches expectations.
                assert_eq!(1, children.len());
                let (child_node_inode, child_node) = children.first().unwrap();
                assert_ne!(ino, *child_node_inode);
                assert_eq!(
                    &proto::node::Node::File(
                        fixtures::DIRECTORY_WITH_KEEP.files.first().unwrap().clone()
                    ),
                    child_node
                );
            }
            InodeData::Regular(..) | InodeData::Symlink(_) => panic!("wrong type"),
        }
    }

    // TODO: insert in the other order
}
