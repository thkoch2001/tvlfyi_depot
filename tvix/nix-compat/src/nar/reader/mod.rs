//! Implements an interface for reaching the Nix archive format (NAR).
//!
//! NAR files (and their hashed representations) are used in C++ Nix for
//! addressing fixed-output derivations and a variety of other things.
use std::io::{
    self,
    ErrorKind::{InvalidData, UnexpectedEof},
    Read,
};

use crate::nar::wire;

mod read;

pub type Reader<'a> = dyn Read + Send + 'a;

/// Start reading a new NAR, reading from the specified reader.
/// It returns a [Node], or an error.
pub fn open<'a, 'r>(reader: &'a mut Reader<'r>) -> io::Result<Node<'a, 'r>> {
    read::token(reader, &wire::TOK_NAR)?;
    Node::new(reader)
}

/// Single node in a NAR file.
///
/// It can either be a Symlink, File or Directory.
/// It is the caller's responsibility to read files and directories to the end,
/// via [FileReader] and [DirReader], respectively.
/// TODO(edef): add debug_assertions to detect wrong usage
pub enum Node<'a, 'r> {
    Symlink {
        target: Vec<u8>,
    },
    File {
        executable: bool,
        reader: FileReader<'a, 'r>,
    },
    Directory(DirReader<'a, 'r>),
}

impl<'a, 'r> Node<'a, 'r> {
    /// Start reading the next node, matching on the next Node tag.
    fn new(reader: &'a mut Reader<'r>) -> io::Result<Self> {
        Ok(match read::tag(reader)? {
            // In the case of a symlink, read the target
            wire::Node::Sym => {
                let target = read::bytes(reader, wire::MAX_TARGET_LEN)?;

                if target.is_empty() || target.contains(&0) {
                    return Err(InvalidData.into());
                }

                // for symlinks, we're done reading everything after having read
                // the target (which we're about to return), so consume the
                // closing paren and return the [Node::Symlink].
                // In the case of files and directories, it's handled by the
                // corresponding readers.
                read::token(reader, &wire::TOK_PAR)?;

                Node::Symlink { target }
            }
            tag @ (wire::Node::Reg | wire::Node::Exe) => {
                let len = read::u64(reader)?; // file length

                Node::File {
                    executable: tag == wire::Node::Exe,
                    reader: FileReader::new(reader, len)?,
                }
            }
            wire::Node::Dir => Node::Directory(DirReader::new(reader)),
        })
    }
}
/// Reads a file from the NAR stream (and the closing paren)
pub struct FileReader<'a, 'r> {
    reader: &'a mut Reader<'r>,
    len: u64,
    pad: u8,
}

impl<'a, 'r> FileReader<'a, 'r> {
    fn new(reader: &'a mut Reader<'r>, len: u64) -> io::Result<Self> {
        // early exit for zero-length files
        if len == 0 {
            read::token(reader, &wire::TOK_PAR)?;
        }

        Ok(Self {
            reader,
            len,
            pad: len as u8,
        })
    }

    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    pub fn len(&self) -> u64 {
        self.len
    }
}

impl Read for FileReader<'_, '_> {
    fn read(&mut self, mut buf: &mut [u8]) -> io::Result<usize> {
        if buf.is_empty() || self.is_empty() {
            return Ok(0);
        }

        if buf.len() as u64 > self.len {
            buf = &mut buf[..self.len as usize];
        }

        let n = self.reader.read(buf)?;
        self.len -= n as u64;

        if n == 0 {
            return Err(UnexpectedEof.into());
        }

        // if we're at the end of the file, also consume the padding,
        // ensure it's all zeroes before returning the number of bytes read.
        if self.is_empty() {
            let pad = (self.pad & 7) as usize;

            if pad != 0 {
                let mut buf = [0; 8];
                self.reader.read_exact(&mut buf[pad..])?;

                if buf != [0; 8] {
                    return Err(InvalidData.into());
                }
            }

            read::token(self.reader, &wire::TOK_PAR)?;
        }

        Ok(n)
    }
}

/// Reads the entries of a directory from the NAR stream
pub struct DirReader<'a, 'r> {
    reader: &'a mut Reader<'r>,
    prev_name: Option<Vec<u8>>,
}

pub struct Entry<'a, 'r> {
    pub name: Vec<u8>,
    pub node: Node<'a, 'r>,
}

impl<'a, 'r> DirReader<'a, 'r> {
    /// initialize a new DirReader.
    fn new(reader: &'a mut Reader<'r>) -> Self {
        Self {
            reader,
            prev_name: None,
        }
    }

    /// returns the next [Entry] from the directory listing.
    /// We don't implement Iterator here, because we can't really treat it like
    /// that, and expecting to be able to use it like that will cause problems:
    /// - we can't consume halfway and be fine
    /// - we can't continue on an error and be fine
    /// - once you get None back, you shouldn't try to keep reading.
    #[allow(clippy::should_implement_trait)]
    pub fn next(&mut self) -> io::Result<Option<Entry>> {
        if self.prev_name.is_some() {
            read::token(self.reader, &wire::TOK_PAR)?;
        }

        // this is either a [wire::Entry::None] or a [wire::Entry::Some],
        // we're done if it's the former.
        if let wire::Entry::None = read::tag(self.reader)? {
            return Ok(None);
        }

        // new entry, read its name.
        let name = read::bytes(self.reader, wire::MAX_NAME_LEN)?;

        // validate the name
        if name.is_empty()
            || name.contains(&0)
            || name.contains(&b'/')
            || name == b"."
            || name == b".."
        {
            return Err(InvalidData.into());
        }

        match &mut self.prev_name {
            None => {
                self.prev_name = Some(name.clone());
            }
            // if there was previous entry, ensure it's sorted.
            Some(prev_name) => {
                if *prev_name >= name {
                    return Err(InvalidData.into());
                }

                prev_name.clear();
                prev_name.extend_from_slice(&name);
            }
        }

        // consume more data until we could match on the next type again.
        read::token(self.reader, &wire::TOK_NOD)?;

        // return the entry.
        Ok(Some(Entry {
            name,
            node: Node::new(&mut self.reader)?,
        }))
    }
}
