use std::io::{
    self,
    ErrorKind::{Interrupted, InvalidData, UnexpectedEof},
};

use super::Reader;
use crate::nar::wire::Tag;

pub fn u64(reader: &mut Reader) -> io::Result<u64> {
    let mut buf = [0; 8];
    reader.read_exact(&mut buf)?;
    Ok(u64::from_le_bytes(buf))
}

pub fn bytes(reader: &mut Reader, max_len: usize) -> io::Result<Vec<u8>> {
    let len = self::u64(reader)?;
    if len > max_len as u64 {
        return Err(InvalidData.into());
    }
    let len = len as usize;

    let buf_len = (len + 7) & !7;
    let mut buf = vec![0; buf_len];
    reader.read_exact(&mut buf)?;

    for b in buf.drain(len..) {
        if b != 0 {
            return Err(InvalidData.into());
        }
    }

    Ok(buf)
}

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
