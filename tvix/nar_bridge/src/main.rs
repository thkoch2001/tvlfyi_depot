use anyhow::Result;
use hash_reader::HashReader;
use nar_bridge::api::{
    nar_info::{HashAlgo, NarHash},
    path_info::Node,
    NarInfo, PathInfo,
};
use nix_nar;
use sha2::Digest;
use std::io::Read;

mod directory_it;
mod hash_reader;
mod util;

// import_nar does read through a reader pointing to NAR file contents.
// It will populate a BlobStore and DirectoryStore while reading through it.
// It will return a (partial) PathInfo object, or an error.
pub fn import_nar<R: Read>(r: R) -> Result<PathInfo, nix_nar::NarError> {
    // set up a sha256 reader that'll hash everything read through it
    let mut sha256_r = HashReader::new(r, sha2::Sha256::new());

    // setup the NAR decoder
    let decoder = nix_nar::Decoder::new(&mut sha256_r)?;
    let mut it = decoder.entries()?.peekable();

    let first_entry = it.next();

    match first_entry {
        Some(Ok(mut e)) => {
            // The first entry always None as path, and there's no way to construct a wrong NAR file with it.
            debug_assert!(e.path.is_none());

            let root_node: Node = match directory_it::convert(&mut e)? {
                // In case the first element was a directory,
                // use the DirectoryIterator to traverse until the end of the NAR.
                Node::Directory(mut n) => {
                    // Construct a DirectoryIterator
                    let directory_it = directory_it::DirectoryIterator::new(it);

                    match directory_it.last() {
                        Some(Ok(d)) => {
                            n.digest = d.digest();
                            n.size = d.size();
                            Node::Directory(n)
                        }
                        Some(Err(e)) => return Err(e),
                        // This has to return at least a single element, as we add the root directory to the stack
                        // when initializing the DirectoryIterator.
                        None => panic!("unreachable"),
                    }
                }
                Node::File(n) => Node::File(n),
                Node::Symlink(n) => Node::Symlink(n),
            };

            let nar_size = sha256_r.bytes_read();

            return Ok(PathInfo {
                node: Some(root_node),
                narinfo: Some(NarInfo {
                    nar_hashes: vec![NarHash {
                        algo: HashAlgo::Sha256 as i32, // TODO: does this get me the right number?
                        digest: sha256_r.digest_bytes().to_owned(),
                    }],
                    nar_size,
                    signatures: vec![],
                    // TODO: run reference scanner, so we can compare it with what's sent in the .narinfo?
                    reference_names: vec![],
                }),
                references: vec![],
            });
        }
        Some(Err(e)) => return Err(e),
        None => {
            return Err(nix_nar::NarError::ParseError(
                "Encountered empty NAR".to_string(),
            ))
        }
    };
}

#[cfg(test)]
mod tests {
    use super::*;

    // The blake3 digest of the 0x01 byte.
    const BLAKE3_DIGEST_0X01: [u8; 32] = [
        0x48, 0xfc, 0x72, 0x1f, 0xbb, 0xc1, 0x72, 0xe0, 0x92, 0x5f, 0xa2, 0x7a, 0xf1, 0x67, 0x1d,
        0xe2, 0x25, 0xba, 0x92, 0x71, 0x34, 0x80, 0x29, 0x98, 0xb1, 0x0a, 0x15, 0x68, 0xa1, 0x88,
        0x65, 0x2b,
    ];

    #[test]
    fn test_emptydirectory() {
        let r = include_bytes!("tests/nar/emptydirectory.nar") as &[u8];
        let res = import_nar(r);
        let pathinfo = res.expect("importer should not fail");
        assert_ne!(pathinfo.node, None, "root node should not be None");
    }

    #[test]
    fn test_onebyteregular() {
        let r = include_bytes!("tests/nar/onebyteregular.nar") as &[u8];
        let res = import_nar(r);
        let pathinfo = res.expect("importer should not fail");
        let root_node = pathinfo.node.expect("root node should not be None");

        match root_node {
            Node::File(f) => {
                assert_eq!(f.name, "", "Name must be empty");
                assert_eq!(f.digest, BLAKE3_DIGEST_0X01, "Digest needs to match");
                assert_eq!(f.size, 1, "Size needs to match");
                assert_eq!(f.executable, false, "May not be executable");
            }
            _ => panic!("invalid root node type"),
        }
    }

    #[test]
    fn test_onebyteexecutable() {
        let r = include_bytes!("tests/nar/onebyteexecutable.nar") as &[u8];
        let res = import_nar(r);
        let pathinfo = res.expect("importer should not fail");
        let root_node = pathinfo.node.expect("root node should not be None");

        match root_node {
            Node::File(f) => {
                assert_eq!(f.name, "", "Name must be empty");
                assert_eq!(f.digest, BLAKE3_DIGEST_0X01, "Digest needs to match");
                assert_eq!(f.size, 1, "Size needs to match");
                assert!(f.executable, "Must be executable");
            }
            _ => panic!("invalid root node type"),
        }
    }

    #[test]
    fn test_symlink() {
        let r = include_bytes!("tests/nar/symlink.nar") as &[u8];
        let res = import_nar(r);
        let pathinfo = res.expect("importer should not fail");
        let root_node = pathinfo.node.expect("root node should not be None");

        match root_node {
            Node::Symlink(f) => {
                assert_eq!(f.name, "", "Name must be empty");
                assert_eq!(
                    f.target, "/nix/store/somewhereelse",
                    "Target must be populated correctly"
                );
            }
            _ => panic!("invalid root node type"),
        }
    }

    #[test]
    fn test_nar_importer() {
        let r = include_bytes!(
            "tests/nar/nar_1094wph9z4nwlgvsd53abfz8i117ykiv5dwnq9nnhz846s7xqd7d.nar"
        ) as &[u8];
        let res = import_nar(r);

        assert_eq!(
            res,
            Ok(PathInfo {
                references: vec![], // TODO
                narinfo: Some(NarInfo {
                    nar_size: 464152,
                    nar_hashes: vec![NarHash {
                        algo: HashAlgo::Sha256 as i32, // TODO: does this get me the right number?
                        digest: vec![
                            0xc6, 0xe1, 0x55, 0xb3, 0x45, 0x6e, 0x30, 0xb7, 0x61, 0x22, 0x63, 0xec,
                            0x09, 0x50, 0x70, 0x81, 0x1c, 0xaf, 0x8a, 0xbf, 0xd5, 0x9f, 0xaa, 0x72,
                            0xab, 0x82, 0xa5, 0x92, 0xef, 0xde, 0xb2, 0x53
                        ],
                    }],
                    signatures: vec![],
                    reference_names: vec![],
                }),
                node: None,
            })
        )
        // TODO: more expectations on pathinfo struct
    }
}
