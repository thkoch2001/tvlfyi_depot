use anyhow::Result;
use std::collections::HashSet;
use thiserror::Error;

use prost::Message;

tonic::include_proto!("tvix.store.v1");

#[derive(Debug, Error, PartialEq)]
pub enum ValidateDirectoryError {
    #[error("{0} is not sorted")]
    WrongSorting(String),
    #[error("{0} is a duplicate name")]
    DuplicateName(String),
    #[error("Invalid name in {0}")]
    InvalidName(String),
    #[error("Ivalid Digest length: {0}")]
    InvalidDigestLen(usize),
}

impl Directory {
    // The size of a directory is the number of all regular and symlink elements,
    // the number of directory elements, and their size fields.
    pub fn size(&self) -> u32 {
        self.files.len() as u32
            + self.symlinks.len() as u32
            + self
                .directories
                .iter()
                .fold(0, |acc: u32, e| (acc + 1 + e.size) as u32)
    }

    pub fn digest(&self) -> Vec<u8> {
        let mut hasher = blake3::Hasher::new();

        hasher.update(&self.encode_to_vec()).finalize().as_bytes()[..].to_vec()
    }

    // name_is_valid checks a name for validity.
    // We disallow slashes, null bytes, . and ..
    // Whether the empty string is fine or not is still up for debate.
    fn name_is_valid(name: &String) -> bool {
        if name == ".." || name == "." || name.contains("\x00") || name.contains("/") {
            return false;
        }
        return true;
    }

    // validate checks the directory for invalid data, such as:
    // - violations of name restrictions
    // - invalid digest lengths
    // - not properly sorted lists
    // - duplicate names in the three lists
    pub fn validate(&self) -> Result<(), ValidateDirectoryError> {
        let mut seen_names = HashSet::new();

        let mut last_directory_name = "".to_string();
        let mut last_file_name = "".to_string();
        let mut last_symlink_name = "".to_string();

        // check directories
        for directory_node in &self.directories {
            // check name for validity
            if !Directory::name_is_valid(&directory_node.name) {
                return Err(ValidateDirectoryError::InvalidName(
                    directory_node.name.clone(),
                ));
            }

            // check digest to be 32 bytes
            if directory_node.digest.len() != 32 {
                return Err(ValidateDirectoryError::InvalidDigestLen(
                    directory_node.digest.len(),
                ));
            }

            // ensure names are sorted
            if directory_node.name < last_directory_name {
                return Err(ValidateDirectoryError::WrongSorting(
                    directory_node.name.clone(),
                ));
            }
            last_directory_name = directory_node.name.clone();

            // ensure names are unique
            if seen_names.get(&directory_node.name).is_some() {
                return Err(ValidateDirectoryError::DuplicateName(
                    directory_node.name.clone(),
                ));
            }
            seen_names.insert(directory_node.name.clone());
        }

        // check files
        for file_node in &self.files {
            // check name for validity
            if !Directory::name_is_valid(&file_node.name) {
                return Err(ValidateDirectoryError::InvalidName(file_node.name.clone()));
            }

            // check digest to be 32 bytes
            if file_node.digest.len() != 32 {
                return Err(ValidateDirectoryError::InvalidDigestLen(
                    file_node.digest.len(),
                ));
            }

            // ensure names are sorted
            if file_node.name < last_file_name {
                return Err(ValidateDirectoryError::WrongSorting(file_node.name.clone()));
            }
            last_file_name = file_node.name.clone();

            // ensure names are unique
            if seen_names.get(&file_node.name).is_some() {
                return Err(ValidateDirectoryError::DuplicateName(
                    file_node.name.clone(),
                ));
            }
            seen_names.insert(file_node.name.clone());
        }

        // check symlinks
        for symlink_node in &self.symlinks {
            // check name for validity
            if !Directory::name_is_valid(&symlink_node.name) {
                return Err(ValidateDirectoryError::InvalidName(
                    symlink_node.name.clone(),
                ));
            }

            // ensure names are sorted
            if symlink_node.name < last_symlink_name {
                return Err(ValidateDirectoryError::WrongSorting(
                    symlink_node.name.clone(),
                ));
            }
            last_symlink_name = symlink_node.name.clone();

            // ensure names are unique
            if seen_names.get(&symlink_node.name).is_some() {
                return Err(ValidateDirectoryError::DuplicateName(
                    symlink_node.name.clone(),
                ));
            }
            seen_names.insert(symlink_node.name.clone());
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::{Directory, DirectoryNode, FileNode, SymlinkNode, ValidateDirectoryError};
    use lazy_static::lazy_static;

    lazy_static! {
        static ref DUMMY_DIGEST: Vec<u8> = vec![
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00,
        ];
    }
    #[test]
    fn test_directory_size() {
        {
            let d = Directory {
                directories: vec![],
                files: vec![],
                symlinks: vec![],
            };
            assert_eq!(d.size(), 0);
        }
        {
            let d = Directory {
                directories: vec![DirectoryNode {
                    name: String::from("foo"),
                    digest: vec![],
                    size: 0,
                }],
                files: vec![],
                symlinks: vec![],
            };
            assert_eq!(d.size(), 1);
        }
        {
            let d = Directory {
                directories: vec![DirectoryNode {
                    name: String::from("foo"),
                    digest: DUMMY_DIGEST.to_vec(),
                    size: 4,
                }],
                files: vec![],
                symlinks: vec![],
            };
            assert_eq!(d.size(), 5);
        }
        {
            let d = Directory {
                directories: vec![],
                files: vec![FileNode {
                    name: String::from("foo"),
                    digest: DUMMY_DIGEST.to_vec(),
                    size: 42,
                    executable: false,
                }],
                symlinks: vec![],
            };
            assert_eq!(d.size(), 1);
        }
        {
            let d = Directory {
                directories: vec![],
                files: vec![],
                symlinks: vec![SymlinkNode {
                    name: String::from("foo"),
                    target: String::from("bar"),
                }],
            };
            assert_eq!(d.size(), 1);
        }
    }

    #[test]
    fn test_digest() {
        let d = Directory {
            directories: vec![],
            files: vec![],
            symlinks: vec![],
        };

        assert_eq!(
            d.digest(),
            vec![
                0xaf, 0x13, 0x49, 0xb9, 0xf5, 0xf9, 0xa1, 0xa6, 0xa0, 0x40, 0x4d, 0xea, 0x36, 0xdc,
                0xc9, 0x49, 0x9b, 0xcb, 0x25, 0xc9, 0xad, 0xc1, 0x12, 0xb7, 0xcc, 0x9a, 0x93, 0xca,
                0xe4, 0x1f, 0x32, 0x62
            ]
        )
    }

    #[test]
    fn test_directory_validate_empty() {
        let d = Directory {
            directories: vec![],
            files: vec![],
            symlinks: vec![],
        };
        assert_eq!(d.validate(), Ok(()));
    }

    #[test]
    fn test_directory_validate_invalid_names() {
        {
            let d = Directory {
                directories: vec![DirectoryNode {
                    name: ".".to_string(),
                    digest: DUMMY_DIGEST.to_vec(),
                    size: 42,
                }],
                files: vec![],
                symlinks: vec![],
            };
            match d.validate().expect_err("must fail") {
                ValidateDirectoryError::InvalidName(n) => {
                    assert_eq!(n, ".")
                }
                _ => panic!("unexpected error"),
            };
        }

        {
            let d = Directory {
                directories: vec![],
                files: vec![FileNode {
                    name: "..".to_string(),
                    digest: DUMMY_DIGEST.to_vec(),
                    size: 42,
                    executable: false,
                }],
                symlinks: vec![],
            };
            match d.validate().expect_err("must fail") {
                ValidateDirectoryError::InvalidName(n) => {
                    assert_eq!(n, "..")
                }
                _ => panic!("unexpected error"),
            };
        }

        {
            let d = Directory {
                directories: vec![],
                files: vec![],
                symlinks: vec![SymlinkNode {
                    name: "\x00".to_string(),
                    target: "foo".to_string(),
                }],
            };
            match d.validate().expect_err("must fail") {
                ValidateDirectoryError::InvalidName(n) => {
                    assert_eq!(n, "\x00")
                }
                _ => panic!("unexpected error"),
            };
        }

        {
            let d = Directory {
                directories: vec![],
                files: vec![],
                symlinks: vec![SymlinkNode {
                    name: "foo/bar".to_string(),
                    target: "foo".to_string(),
                }],
            };
            match d.validate().expect_err("must fail") {
                ValidateDirectoryError::InvalidName(n) => {
                    assert_eq!(n, "foo/bar")
                }
                _ => panic!("unexpected error"),
            };
        }
    }

    #[test]
    fn test_directory_validate_invalid_digest() {
        let d = Directory {
            directories: vec![DirectoryNode {
                name: "foo".to_string(),
                digest: vec![],
                size: 42,
            }],
            files: vec![],
            symlinks: vec![],
        };
        match d.validate().expect_err("must fail") {
            ValidateDirectoryError::InvalidDigestLen(n) => {
                assert_eq!(n, 0)
            }
            _ => panic!("unexpected error"),
        }
    }

    #[test]
    fn test_directory_validate_sorting() {
        // "b" comes before "a", bad.
        {
            let d = Directory {
                directories: vec![
                    DirectoryNode {
                        name: "b".to_string(),
                        digest: DUMMY_DIGEST.to_vec(),
                        size: 42,
                    },
                    DirectoryNode {
                        name: "a".to_string(),
                        digest: DUMMY_DIGEST.to_vec(),
                        size: 42,
                    },
                ],
                files: vec![],
                symlinks: vec![],
            };
            match d.validate().expect_err("must fail") {
                ValidateDirectoryError::WrongSorting(s) => {
                    assert_eq!(s, "a".to_string());
                }
                _ => panic!("unexpected error"),
            }
        }

        // "a" exists twice, bad.
        {
            let d = Directory {
                directories: vec![
                    DirectoryNode {
                        name: "a".to_string(),
                        digest: DUMMY_DIGEST.to_vec(),
                        size: 42,
                    },
                    DirectoryNode {
                        name: "a".to_string(),
                        digest: DUMMY_DIGEST.to_vec(),
                        size: 42,
                    },
                ],
                files: vec![],
                symlinks: vec![],
            };
            match d.validate().expect_err("must fail") {
                ValidateDirectoryError::DuplicateName(s) => {
                    assert_eq!(s, "a".to_string());
                }
                _ => panic!("unexpected error"),
            }
        }

        // "a" comes before "b", all good.
        {
            let d = Directory {
                directories: vec![
                    DirectoryNode {
                        name: "a".to_string(),
                        digest: DUMMY_DIGEST.to_vec(),
                        size: 42,
                    },
                    DirectoryNode {
                        name: "b".to_string(),
                        digest: DUMMY_DIGEST.to_vec(),
                        size: 42,
                    },
                ],
                files: vec![],
                symlinks: vec![],
            };

            d.validate().expect("validate shouldn't error");
        }

        // [b, c] and [a] are both properly sorted.
        {
            let d = Directory {
                directories: vec![
                    DirectoryNode {
                        name: "b".to_string(),
                        digest: DUMMY_DIGEST.to_vec(),
                        size: 42,
                    },
                    DirectoryNode {
                        name: "c".to_string(),
                        digest: DUMMY_DIGEST.to_vec(),
                        size: 42,
                    },
                ],
                files: vec![],
                symlinks: vec![SymlinkNode {
                    name: "a".to_string(),
                    target: "foo".to_string(),
                }],
            };

            d.validate().expect("validate shouldn't error");
        }
    }
}
