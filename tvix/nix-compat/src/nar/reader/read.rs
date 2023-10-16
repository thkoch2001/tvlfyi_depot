use std::io::{
    self,
    ErrorKind::{Interrupted, InvalidData, UnexpectedEof},
};

use super::Reader;
use crate::nar::wire::Tag;

/// Consume a u64 from the reader and return.
pub fn u64(reader: &mut Reader) -> io::Result<u64> {
    let mut buf = [0; 8];
    reader.read_exact(&mut buf)?;
    Ok(u64::from_le_bytes(buf))
}

/// Consumes a byte string of up to max_len bytes and return.
pub fn bytes(reader: &mut Reader, max_len: usize) -> io::Result<Vec<u8>> {
    // read the length field, reject if it's bigger than allowed.
    let len = self::u64(reader)?;
    if len > max_len as u64 {
        return Err(InvalidData.into());
    }
    // we know len fits in a usize now.
    let len = len as usize;

    // allocate a buf for it, and read data into it.
    let buf_len = (len + 7) & !7; // padded length
    let mut buf = vec![0; buf_len];
    reader.read_exact(&mut buf)?;

    // verify the padding is all zeros and drain it from the returned buf.
    for b in buf.drain(len..) {
        if b != 0 {
            return Err(InvalidData.into());
        }
    }

    Ok(buf)
}

/// Consume a known token from the reader.
pub fn token<const N: usize>(reader: &mut Reader, token: &[u8; N]) -> io::Result<()> {
    let mut token = &token[..];
    let mut buf = [0u8; N];
    let mut buf = &mut buf[..];

    while !token.is_empty() {
        match reader.read(buf) {
            Ok(0) => {
                return Err(UnexpectedEof.into());
            }
            Ok(n) => {
                let (t, b);
                (t, token) = token.split_at(n);
                (b, buf) = buf.split_at_mut(n);

                if t != b {
                    return Err(InvalidData.into());
                }
            }
            Err(e) => {
                if e.kind() != Interrupted {
                    return Err(e);
                }
            }
        }
    }

    Ok(())
}

/// Read from the reader, return a Tag.
/// We read two times, first we read [T::MIN] bytes.
/// Compare by peeking at the discriminator position,
/// then compare all bytes read.
/// If this was all of the token size, we're already done.
/// Otherwise, read and compare the rest.
pub fn tag<T: Tag>(reader: &mut Reader) -> io::Result<T> {
    let mut buf = T::make_buf();
    let buf = buf.as_mut();

    reader.read_exact(&mut buf[..T::MIN])?;

    let tag = T::from_u8(buf[T::OFF]).ok_or(InvalidData)?;
    let (head, tail) = tag.as_bytes().split_at(T::MIN);

    if buf[..T::MIN] != *head {
        return Err(InvalidData.into());
    }

    if !tail.is_empty() {
        let rest = tail.len();
        reader.read_exact(&mut buf[..rest])?;

        if buf[..rest] != *tail {
            return Err(InvalidData.into());
        }
    }

    Ok(tag)
}
