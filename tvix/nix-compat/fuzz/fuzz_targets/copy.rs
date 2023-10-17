#![no_main]

use libfuzzer_sys::fuzz_target;
use nix_compat::nar::{reader, writer};
use std::io::{self, BufReader, Write};

fuzz_target!(|data: &[u8]| {
    let _ = run(data);
});

fn run(data: &[u8]) -> io::Result<()> {
    let mut reader = io::Cursor::new(data);
    let mut writer = Writer(data);

    copy(reader::open(&mut reader)?, writer::open(&mut writer)?)?;

    let off = reader.position();
    let unread = &data[off as usize..];
    let unwritten = writer.0;

    assert_eq!(unread.as_ptr(), unwritten.as_ptr());

    Ok(())
}

struct Writer<'a>(&'a [u8]);

impl Write for Writer<'_> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.0 = self.0.strip_prefix(buf).unwrap();
        Ok(buf.len())
    }

    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}

fn copy(reader: reader::Node, writer: writer::Node) -> io::Result<()> {
    match reader {
        reader::Node::Symlink { target } => writer.symlink(&target),
        reader::Node::File { executable, reader } => {
            writer.file(executable, reader.len(), &mut BufReader::new(reader))
        }
        reader::Node::Directory(mut reader) => {
            let mut writer = writer.directory()?;

            while let Some(entry) = reader.next()? {
                copy(entry.node, writer.entry(&entry.name)?)?;
            }

            writer.close()
        }
    }
}
