use std::io::{
    self,
    ErrorKind::{InvalidData, UnexpectedEof},
    Read,
};

use crate::nar::wire;

mod read;

pub type Reader<'a> = dyn Read + Send + 'a;

pub fn open<'a, 'r>(reader: &'a mut Reader<'r>) -> io::Result<Node<'a, 'r>> {
    read::token(reader, &wire::TOK_NAR)?;
    Node::new(reader)
}

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
    fn new(reader: &'a mut Reader<'r>) -> io::Result<Self> {
        Ok(match read::tag(reader)? {
            wire::Node::Sym => {
                let target = read::bytes(reader, wire::MAX_TARGET_LEN)?;

                if target.is_empty() || target.contains(&0) {
                    return Err(InvalidData.into());
                }

                read::token(reader, &wire::TOK_PAR)?;

                Node::Symlink { target }
            }
            tag @ (wire::Node::Reg | wire::Node::Exe) => {
                let len = read::u64(reader)?;

                Node::File {
                    executable: tag == wire::Node::Exe,
                    reader: FileReader::new(reader, len)?,
                }
            }
            wire::Node::Dir => Node::Directory(DirReader::new(reader)),
        })
    }
}

pub struct FileReader<'a, 'r> {
    reader: &'a mut Reader<'r>,
    len: u64,
    pad: u8,
}

impl<'a, 'r> FileReader<'a, 'r> {
    fn new(reader: &'a mut Reader<'r>, len: u64) -> io::Result<Self> {
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

pub struct DirReader<'a, 'r> {
    reader: &'a mut Reader<'r>,
    prev_name: Option<Vec<u8>>,
}

pub struct Entry<'a, 'r> {
    pub name: Vec<u8>,
    pub node: Node<'a, 'r>,
}

impl<'a, 'r> DirReader<'a, 'r> {
    fn new(reader: &'a mut Reader<'r>) -> Self {
        Self {
            reader,
            prev_name: None,
        }
    }

    #[allow(clippy::should_implement_trait)]
    pub fn next(&mut self) -> io::Result<Option<Entry>> {
        if self.prev_name.is_some() {
            read::token(self.reader, &wire::TOK_PAR)?;
        }

        if let wire::Entry::None = read::tag(self.reader)? {
            return Ok(None);
        }

        let name = read::bytes(self.reader, wire::MAX_NAME_LEN)?;

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
            Some(prev_name) => {
                if *prev_name >= name {
                    return Err(InvalidData.into());
                }

                prev_name.clear();
                prev_name.extend_from_slice(&name);
            }
        }

        read::token(self.reader, &wire::TOK_NOD)?;

        Ok(Some(Entry {
            name,
            node: Node::new(&mut self.reader)?,
        }))
    }
}
