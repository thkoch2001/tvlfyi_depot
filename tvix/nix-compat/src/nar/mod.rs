use std::io::{self, BufReader};

mod wire;

pub mod reader;
pub mod writer;

pub fn copy(reader: reader::Node, writer: writer::Node) -> io::Result<()> {
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
