use tokio::io::{self, AsyncBufRead, ErrorKind::InvalidData};

// Required reading for understanding this module.
use crate::{
    nar,
    wire::{self, BytesReader},
};

mod read;

pub type Reader<'a> = dyn AsyncBufRead + Unpin + Send + 'a;

/// Start reading a NAR file from `reader`.
pub async fn open<'a, 'r>(reader: &'a mut Reader<'r>) -> io::Result<Node<'a, 'r>> {
    read::token(reader, &nar::wire::TOK_NAR).await?;
    Node::new(reader).await
}

pub enum Node<'a, 'r: 'a> {
    Symlink {
        target: Vec<u8>,
    },
    File {
        executable: bool,
        size: u64,
        reader: BytesReader<&'a mut Reader<'r>, nar::wire::PadPar>,
    },
    Directory(DirReader<'a, 'r>),
}

impl<'a, 'r: 'a> Node<'a, 'r> {
    async fn new(reader: &'a mut Reader<'r>) -> io::Result<Self> {
        Ok(match read::tag(reader).await? {
            nar::wire::Node::Sym => {
                let target = wire::read_bytes(reader, 1..=nar::wire::MAX_TARGET_LEN as u64).await?;

                if target.contains(&0) {
                    return Err(InvalidData.into());
                }

                Node::Symlink { target }
            }
            tag @ (nar::wire::Node::Reg | nar::wire::Node::Exe) => {
                let size = wire::read_u64(reader).await?;

                Node::File {
                    executable: tag == nar::wire::Node::Exe,
                    size,
                    reader: wire::BytesReader::with_size(reader, size),
                }
            }
            nar::wire::Node::Dir => Node::Directory(DirReader::new(reader)),
        })
    }
}

pub struct DirReader<'a, 'r> {
    reader: &'a mut Reader<'r>,
    /// Previous directory entry name.
    /// We have to hang onto this to enforce name monotonicity.
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
    pub async fn next(&mut self) -> io::Result<Option<Entry<'_, 'r>>> {
        // COME FROM the previous iteration: if we've already read an entry,
        // read its terminating TOK_PAR here.
        if self.prev_name.is_some() {
            read::token(self.reader, &nar::wire::TOK_PAR).await?;
        }

        if let nar::wire::Entry::None = read::tag(self.reader).await? {
            return Ok(None);
        }

        let name = wire::read_bytes(self.reader, 1..=nar::wire::MAX_NAME_LEN as u64).await?;

        if name.contains(&0) || name.contains(&b'/') || name == b"." || name == b".." {
            return Err(InvalidData.into());
        }

        // Enforce strict monotonicity of directory entry names.
        match &mut self.prev_name {
            None => {
                self.prev_name = Some(name.clone());
            }
            Some(prev_name) => {
                if *prev_name >= name {
                    return Err(InvalidData.into());
                }

                name[..].clone_into(prev_name);
            }
        }

        read::token(self.reader, &nar::wire::TOK_NOD).await?;

        Ok(Some(Entry {
            name,
            node: Node::new(self.reader).await?,
        }))
    }
}
