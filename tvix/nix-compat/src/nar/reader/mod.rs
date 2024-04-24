//! Parser for the Nix archive format, aka NAR.
//!
//! NAR files (and their hashed representations) are used in C++ Nix for
//! a variety of things, including addressing fixed-output derivations
//! and transferring store paths between Nix stores.

use std::io::{
    self, BufRead,
    ErrorKind::{InvalidData, UnexpectedEof},
    Read, Write,
};
#[cfg(debug_assertions)]
use std::{cell::Cell, rc::Rc};

// Required reading for understanding this module.
use crate::nar::wire;

mod read;
#[cfg(test)]
mod test;

pub type Reader<'a> = dyn BufRead + Send + 'a;

pub struct ArchiveReader<'a, 'r> {
    inner: &'a mut Reader<'r>,

    /// In debug mode, also track when we need to abandon this archive reader.
    /// The archive reader must be abandoned when:
    ///   * An error is encountered at any point
    ///   * A file or directory reader is dropped before being read entirely.
    /// All of these checks vanish in release mode, so we just use a reference-counted flag for simplicity.
    #[cfg(debug_assertions)]
    poisoned: Rc<Cell<bool>>,
}

impl<'a, 'r> ArchiveReader<'a, 'r> {
    #[inline(always)]
    fn check_abandon(&self) {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                !self.poisoned.get(),
                "Archive reader used after it was meant to be abandoned!"
            );
        }
    }
}

macro_rules! poison {
    ($it:expr) => {
        #[cfg(debug_assertions)]
        {
            $it.poisoned.set(true);
        }
    };
}

macro_rules! try_or_poison {
    ($it:expr, $ex:expr) => {
        match $ex {
            Ok(x) => x,
            Err(e) => {
                poison!($it);
                return Err(e.into());
            }
        }
    };
}

/// Start reading a NAR file from `reader`.
pub fn open<'a, 'r>(reader: &'a mut Reader<'r>) -> io::Result<Node<'a, 'r>> {
    read::token(reader, &wire::TOK_NAR)?;
    Node::new(ArchiveReader {
        inner: reader,
        #[cfg(debug_assertions)]
        poisoned: Rc::new(Cell::new(false)),
    })
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
    /// Start reading a [Node], matching the next [wire::Node].
    ///
    /// Reading the terminating [wire::TOK_PAR] is done immediately for [Node::Symlink],
    /// but is otherwise left to [DirReader] or [FileReader].
    fn new(reader: ArchiveReader<'a, 'r>) -> io::Result<Self> {
        Ok(match read::tag(reader.inner)? {
            wire::Node::Sym => {
                let target = read::bytes(reader.inner, wire::MAX_TARGET_LEN)?;

                if target.is_empty() || target.contains(&0) {
                    return Err(InvalidData.into());
                }

                read::token(reader.inner, &wire::TOK_PAR)?;

                Node::Symlink { target }
            }
            tag @ (wire::Node::Reg | wire::Node::Exe) => {
                let len = read::u64(reader.inner)?;

                Node::File {
                    executable: tag == wire::Node::Exe,
                    reader: FileReader::new(reader, len)?,
                }
            }
            wire::Node::Dir => Node::Directory(DirReader::new(reader)),
        })
    }
}

/// File contents, readable through the [Read] trait.
///
/// It comes with some caveats:
///  * You must always read the entire file, unless you intend to abandon the entire archive reader.
///  * You must abandon the entire archive reader upon the first error.
///
/// It's fine to read exactly `reader.len()` bytes without ever seeing an explicit EOF.
pub struct FileReader<'a, 'r> {
    reader: ArchiveReader<'a, 'r>,
    len: u64,
    /// Truncated original file length for padding computation.
    /// We only care about the 3 least significant bits; semantically, this is a u3.
    pad: u8,
}

impl<'a, 'r> FileReader<'a, 'r> {
    /// Instantiate a new reader, starting after [wire::TOK_REG] or [wire::TOK_EXE].
    /// We handle the terminating [wire::TOK_PAR] on semantic EOF.
    fn new(reader: ArchiveReader<'a, 'r>, len: u64) -> io::Result<Self> {
        // For zero-length files, we have to read the terminating TOK_PAR
        // immediately, since FileReader::read may never be called; we've
        // already reached semantic EOF by definition.
        if len == 0 {
            read::token(reader.inner, &wire::TOK_PAR)?;
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

impl FileReader<'_, '_> {
    /// Equivalent to [BufRead::fill_buf]
    ///
    /// We can't directly implement [BufRead], because [FileReader::consume] needs
    /// to perform fallible I/O.
    pub fn fill_buf(&mut self) -> io::Result<&[u8]> {
        self.reader.check_abandon();

        if self.is_empty() {
            return Ok(&[]);
        }

        let mut buf = try_or_poison!(self.reader, self.reader.inner.fill_buf());

        if buf.is_empty() {
            poison!(self.reader);
            return Err(UnexpectedEof.into());
        }

        if buf.len() as u64 > self.len {
            buf = &buf[..self.len as usize];
        }

        Ok(buf)
    }

    /// Analogous to [BufRead::consume], differing only in that it needs
    /// to perform I/O in order to read padding and terminators.
    pub fn consume(&mut self, n: usize) -> io::Result<()> {
        self.reader.check_abandon();

        if n == 0 {
            return Ok(());
        }

        self.len = self
            .len
            .checked_sub(n as u64)
            .expect("consumed bytes past EOF");

        self.reader.inner.consume(n);

        if self.is_empty() {
            self.finish()?;
        }

        Ok(())
    }

    /// Copy the (remaining) contents of the file into `dst`.
    pub fn copy(&mut self, mut dst: impl Write) -> io::Result<()> {
        self.reader.check_abandon();

        while !self.is_empty() {
            let buf = self.fill_buf()?;
            let n = try_or_poison!(self.reader, dst.write(buf));
            self.consume(n)?;
        }

        Ok(())
    }
}

impl Read for FileReader<'_, '_> {
    fn read(&mut self, mut buf: &mut [u8]) -> io::Result<usize> {
        self.reader.check_abandon();

        if buf.is_empty() || self.is_empty() {
            return Ok(0);
        }

        if buf.len() as u64 > self.len {
            buf = &mut buf[..self.len as usize];
        }

        let n = try_or_poison!(self.reader, self.reader.inner.read(buf));
        self.len -= n as u64;

        if n == 0 {
            poison!(self.reader);
            return Err(UnexpectedEof.into());
        }

        if self.is_empty() {
            self.finish()?;
        }

        Ok(n)
    }
}

#[cfg(debug_assertions)]
impl Drop for FileReader<'_, '_> {
    fn drop(&mut self) {
        if !self.reader.poisoned.get() && !self.is_empty() {
            // We stopped reading halfway through, so we mustn't use this archive reader again
            poison!(self.reader);
        }
    }
}

impl FileReader<'_, '_> {
    /// We've reached semantic EOF, consume and verify the padding and terminating TOK_PAR.
    /// Files are padded to 64 bits (8 bytes), just like any other byte string in the wire format.
    fn finish(&mut self) -> io::Result<()> {
        self.reader.check_abandon();

        let pad = (self.pad & 7) as usize;

        if pad != 0 {
            let mut buf = [0; 8];
            try_or_poison!(self.reader, self.reader.inner.read_exact(&mut buf[pad..]));

            if buf != [0; 8] {
                poison!(self.reader);
                return Err(InvalidData.into());
            }
        }

        read::token(self.reader.inner, &wire::TOK_PAR)
    }
}

/// A directory iterator, yielding a sequence of [Node]s.
/// It must be fully consumed before reading further from the [DirReader] that produced it, if any.
pub struct DirReader<'a, 'r> {
    reader: ArchiveReader<'a, 'r>,
    /// Previous directory entry name.
    /// We have to hang onto this to enforce name monotonicity.
    prev_name: Option<Vec<u8>>,

    #[cfg(debug_assertions)]
    finished: bool,
}

pub struct Entry<'a, 'r> {
    pub name: Vec<u8>,
    pub node: Node<'a, 'r>,
}

impl<'a, 'r> DirReader<'a, 'r> {
    fn new(reader: ArchiveReader<'a, 'r>) -> Self {
        Self {
            reader,
            prev_name: None,
            #[cfg(debug_assertions)]
            finished: false,
        }
    }

    /// Read the next [Entry] from the directory.
    ///
    /// We explicitly don't implement [Iterator], since treating this as
    /// a regular Rust iterator will surely lead you astray.
    ///
    ///  * You must always consume the entire iterator, unless you abandon the entire archive reader.
    ///  * You must abandon the entire archive reader on the first error.
    ///  * You must abandon the directory reader upon the first [None].
    ///  * Even if you know the amount of elements up front, you must keep reading until you encounter [None].
    #[allow(clippy::should_implement_trait)]
    pub fn next(&mut self) -> io::Result<Option<Entry<'_, 'r>>> {
        self.reader.check_abandon();
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                !self.finished,
                "DirReader called again after returning None"
            );
        }

        // COME FROM the previous iteration: if we've already read an entry,
        // read its terminating TOK_PAR here.
        if self.prev_name.is_some() {
            try_or_poison!(self.reader, read::token(self.reader.inner, &wire::TOK_PAR));
        }

        // Determine if there are more entries to follow
        if let wire::Entry::None = try_or_poison!(self.reader, read::tag(self.reader.inner)) {
            // We've reached the end of this directory.
            #[cfg(debug_assertions)]
            {
                self.finished = true;
            }
            return Ok(None);
        }

        let name = try_or_poison!(
            self.reader,
            read::bytes(self.reader.inner, wire::MAX_NAME_LEN)
        );

        if name.is_empty()
            || name.contains(&0)
            || name.contains(&b'/')
            || name == b"."
            || name == b".."
        {
            poison!(self.reader);
            return Err(InvalidData.into());
        }

        // Enforce strict monotonicity of directory entry names.
        match &mut self.prev_name {
            None => {
                self.prev_name = Some(name.clone());
            }
            Some(prev_name) => {
                if *prev_name >= name {
                    poison!(self.reader);
                    return Err(InvalidData.into());
                }

                name[..].clone_into(prev_name);
            }
        }

        try_or_poison!(self.reader, read::token(self.reader.inner, &wire::TOK_NOD));

        Ok(Some(Entry {
            name,
            node: try_or_poison!(
                self.reader,
                Node::new(ArchiveReader {
                    inner: self.reader.inner,
                    #[cfg(debug_assertions)]
                    poisoned: self.reader.poisoned.clone(),
                })
            ),
        }))
    }
}

#[cfg(debug_assertions)]
impl Drop for DirReader<'_, '_> {
    fn drop(&mut self) {
        if !self.reader.poisoned.get() && !self.finished {
            // We stopped reading halfway through, so we mustn't use this archive reader again
            poison!(self.reader);
        }
    }
}
