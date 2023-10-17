#![no_main]

use libfuzzer_sys::fuzz_target;
use nix_compat::nar::reader::*;
use std::io;

fuzz_target!(|data: &[u8]| {
    let mut reader = io::Cursor::new(data);
    let _ = open(&mut reader).and_then(walk);
});

fn walk(node: Node) -> io::Result<()> {
    match node {
        Node::Symlink { .. } => {}
        Node::File { mut reader, .. } => {
            let len = reader.len();
            let n = io::copy(&mut reader, &mut io::sink())?;
            assert_eq!(len, n);
        }
        Node::Directory(mut reader) => {
            while let Some(node) = reader.next()? {
                walk(node.node)?;
            }
        }
    }

    Ok(())
}
