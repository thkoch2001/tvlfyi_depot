use std::io;
use std::path::PathBuf;
use std::{io::Read, iter::Peekable};

use anyhow::Result;
use nar_bridge::api::{path_info::Node, Directory, DirectoryNode};
use nar_bridge::api::{FileNode, SymlinkNode};

use crate::util::basename_if_exists;

pub struct DirectoryIterator<'a, R: Read> {
    stack: Vec<(PathBuf, Directory)>,
    it: Peekable<nix_nar::Entries<'a, R>>,
    end_of_nar: bool,
}

impl<R: Read> DirectoryIterator<'_, R> {
    pub fn new(nar_it: Peekable<nix_nar::Entries<R>>) -> DirectoryIterator<R> {
        return DirectoryIterator {
            stack: vec![(
                PathBuf::from("/"),
                Directory {
                    directories: vec![],
                    files: vec![],
                    symlinks: vec![],
                },
            )],
            it: nar_it,
            end_of_nar: false,
        };
    }

    // This pops the head of the stack off and returns it.
    // If the stack was already empty, this returns None.
    // If there's anything left on the stack (a parent Directory),
    // add a reference to the popped Directory into its directories list.
    fn pop_from_stack(&mut self) -> Option<Directory> {
        let popped = self.stack.pop();

        match popped {
            None => None,
            Some(popped) => {
                // check for a parent directory, which is now the top of the stack
                match self.stack.last_mut() {
                    // if there is one, add a reference to the popped Directory
                    Some(parent) => {
                        // calculate the ref of popped
                        parent.1.directories.push(DirectoryNode {
                            name: basename_if_exists(Some(&popped.0)),
                            digest: popped.1.digest(),
                            size: popped.1.size(),
                        });
                    }
                    None => {}
                };
                Some(popped.1)
            }
        }
    }
}

impl<R: Read> Iterator for DirectoryIterator<'_, R> {
    type Item = Result<Directory, nix_nar::NarError>;

    fn next(&mut self) -> Option<Self::Item> {
        // If we're at the end of the NAR, we only pop elements from the stack.
        if self.end_of_nar {
            // pop an element from the stack, while adding a ref to possible parents,
            // until there's no elements left, then we yield None.
            return match self.pop_from_stack() {
                Some(d) => Some(Ok(d)),
                None => None,
            };
        }

        loop {
            // TODO: add debug_asserts in the iterator for path.is_some()

            // We're peeking here, to avoid advancing the iterator if we're
            // only dropping off things from the stack.
            match self.it.peek() {
                // No more elements from the iterator, start popping
                None => {
                    self.end_of_nar = true;
                    return match self.pop_from_stack() {
                        Some(d) => Some(Ok(d)),
                        None => None,
                    };
                }
                // More elements coming from the iterator
                Some(entry) => {
                    // an entry coming from the iterator.
                    match entry {
                        // pass parsing errors along
                        Err(_) => {
                            // we need to consume from the iterator instead of just peeking,
                            // because of ownership
                            let entry = self.it.next();
                            // we know it's a Some, and it's an err,
                            // because that's the branches we're in.
                            let e = entry.unwrap().err().unwrap();
                            return Some(Err(e));
                        }

                        Ok(e) => {
                            // Look at the path of the element received.
                            // If it's pointing 'depeer', it can only be one level down
                            // (as we receive intermediate Directory entries)

                            // If it's pointing up, it can "skip" multiple levels.
                            // We don't care, we simply pop directories from the stack
                            // until it's not pointing up anymore.
                            let top_of_stack = self
                                .stack
                                .last_mut()
                                .expect("stack may not be empty at this point");

                            // pointing up. pop the element from stack.
                            if !e.abs_path().starts_with(&top_of_stack.0) {
                                return match self.pop_from_stack() {
                                    Some(d) => Some(Ok(d)),
                                    None => None,
                                };
                            }

                            // pointing down
                            // TODO: assert broken logic here. Make sure this is a direct child.

                            // consume from the iterator. We know there is something.
                            let mut e = match self.it.next().unwrap() {
                                Ok(e) => e,
                                Err(e) => return Some(Err(e)),
                            };
                            // convert entry to node. Convert will also read data of regular files.
                            let node = match convert(&mut e) {
                                Ok(n) => n,
                                Err(e) => return Some(Err(e)),
                            };

                            match node {
                                Node::Directory(_n) => {
                                    // open a new stack frame, with an empty Directory object.
                                    self.stack.push((
                                        e.path.unwrap(),
                                        Directory {
                                            directories: vec![],
                                            files: vec![],
                                            symlinks: vec![],
                                        },
                                    ))
                                }
                                Node::File(n) => {
                                    top_of_stack.1.files.push(n);
                                }
                                Node::Symlink(n) => {
                                    top_of_stack.1.symlinks.push(n);
                                }
                            };
                        }
                    }
                }
            }
        }
    }
}

// This converts a nix_nar::Entry to a pathinfo::Node, or an error.
// For directory nodes, digest and size is not populated,
// as we'll only know once we're "coming back" after having seen all child elements.
// TODO: better function name
// TODO: reference to blob store?
pub fn convert<R: Read>(entry: &mut nix_nar::Entry<R>) -> Result<Node, nix_nar::NarError> {
    match &mut entry.content {
        nix_nar::Content::Directory => Ok(Node::Directory(DirectoryNode {
            name: basename_if_exists(entry.path.as_deref()),
            digest: vec![],
            size: 0,
        })),
        nix_nar::Content::Symlink { target } => Ok(Node::Symlink(SymlinkNode {
            name: basename_if_exists(entry.path.as_deref()),
            target: target.as_os_str().to_string_lossy().to_string(),
        })),
        nix_nar::Content::File {
            executable,
            size,
            offset: _,
            ref mut data,
        } => {
            // TODO: put into BlobStore
            // TODO: use rayon for large chunks?

            let mut hasher = blake3::Hasher::new();

            match io::copy(data, &mut hasher) {
                Ok(num_bytes) => {
                    if num_bytes as u64 != *size {
                        return Err(nix_nar::NarError::ParseError(
                            "invalid content size".to_string(),
                        ));
                    }
                }
                Err(e) => {
                    return Err(nix_nar::NarError::ParseError(format!(
                        "Error reading file contents: {}",
                        e.to_string()
                    )))
                }
            };
            let digest = hasher.finalize().as_bytes().to_vec();

            Ok(Node::File(FileNode {
                name: basename_if_exists(entry.path.as_deref()),
                digest,
                size: *size as u32,
                executable: *executable,
            }))
        }
    }
}
