#[cfg(feature = "async")]
use std::mem::MaybeUninit;
use std::{
    io::{Error, ErrorKind},
    ops::RangeInclusive,
};
#[cfg(feature = "async")]
use tokio::io::ReadBuf;
use tokio::io::{self, AsyncReadExt, AsyncWriteExt};

pub(crate) mod reader;
pub use reader::BytesReader;
mod writer;
pub use writer::BytesWriter;

/// 8 null bytes, used to write out padding.
pub(crate) const EMPTY_BYTES: &[u8; 8] = &[0u8; 8];

/// The length of the size field, in bytes is always 8.
const LEN_SIZE: usize = 8;

/// Read a "bytes wire packet" from the AsyncRead.
/// Rejects reading more than `allowed_size` bytes of payload.
///
/// The packet is made up of three parts:
/// - a length header, u64, LE-encoded
/// - the payload itself
/// - null bytes to the next 8 byte boundary
///
/// Ensures the payload size fits into the `allowed_size` passed,
/// and that the padding is actual null bytes.
///
/// On success, the returned `Vec<u8>` only contains the payload itself.
/// On failure (for example if a too large byte packet was sent), the reader
/// becomes unusable.
///
/// This buffers the entire payload into memory,
/// a streaming version is available at [crate::wire::bytes::BytesReader].
pub async fn read_bytes<R>(r: &mut R, allowed_size: RangeInclusive<usize>) -> io::Result<Vec<u8>>
where
    R: AsyncReadExt + Unpin + ?Sized,
{
    // read the length field
    let len = r.read_u64_le().await?;
    let len: usize = len
        .try_into()
        .ok()
        .filter(|len| allowed_size.contains(len))
        .ok_or_else(|| {
            io::Error::new(
                io::ErrorKind::InvalidData,
                "signalled package size not in allowed range",
            )
        })?;

    // calculate the total length, including padding.
    // byte packets are padded to 8 byte blocks each.
    let padded_len = padding_len(len as u64) as u64 + (len as u64);
    let mut limited_reader = r.take(padded_len);

    let mut buf = Vec::new();

    let s = limited_reader.read_to_end(&mut buf).await?;

    // make sure we got exactly the number of bytes, and not less.
    if s as u64 != padded_len {
        return Err(io::ErrorKind::UnexpectedEof.into());
    }

    let (_content, padding) = buf.split_at(len);

    // ensure the padding is all zeroes.
    if padding.iter().any(|&b| b != 0) {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            "padding is not all zeroes",
        ));
    }

    // return the data without the padding
    buf.truncate(len);
    Ok(buf)
}

#[cfg(feature = "async")]
pub(crate) async fn read_bytes_buf<'a, const N: usize, R>(
    reader: &mut R,
    buf: &'a mut [MaybeUninit<u8>; N],
    allowed_size: RangeInclusive<usize>,
) -> io::Result<&'a [u8]>
where
    R: AsyncReadExt + Unpin + ?Sized,
{
    assert_eq!(N % 8, 0);
    assert!(*allowed_size.end() <= N);

    let len = reader.read_u64_le().await?;
    let len: usize = len
        .try_into()
        .ok()
        .filter(|len| allowed_size.contains(len))
        .ok_or_else(|| {
            io::Error::new(
                io::ErrorKind::InvalidData,
                "signalled package size not in allowed range",
            )
        })?;

    let buf_len = (len + 7) & !7;
    let buf = {
        let mut read_buf = ReadBuf::uninit(&mut buf[..buf_len]);

        while read_buf.filled().len() < buf_len {
            reader.read_buf(&mut read_buf).await?;
        }

        // ReadBuf::filled does not pass the underlying buffer's lifetime through,
        // so we must make a trip to hell.
        //
        // SAFETY: `read_buf` is filled up to `buf_len`, and we verify that it is
        // still pointing at the same underlying buffer.
        unsafe {
            assert_eq!(read_buf.filled().as_ptr(), buf.as_ptr() as *const u8);
            assume_init_bytes(&buf[..buf_len])
        }
    };

    if buf[len..buf_len].iter().any(|&b| b != 0) {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            "padding is not all zeroes",
        ));
    }

    Ok(&buf[..len])
}

/// SAFETY: The bytes have to actually be initialized.
#[cfg(feature = "async")]
unsafe fn assume_init_bytes(slice: &[MaybeUninit<u8>]) -> &[u8] {
    &*(slice as *const [MaybeUninit<u8>] as *const [u8])
}

/// Read a "bytes wire packet" of from the AsyncRead and tries to parse as string.
/// Internally uses [read_bytes].
/// Rejects reading more than `allowed_size` bytes of payload.
pub async fn read_string<R>(r: &mut R, allowed_size: RangeInclusive<usize>) -> io::Result<String>
where
    R: AsyncReadExt + Unpin,
{
    let bytes = read_bytes(r, allowed_size).await?;
    String::from_utf8(bytes).map_err(|e| Error::new(ErrorKind::InvalidData, e))
}

/// Writes a "bytes wire packet" to a (hopefully buffered) [AsyncWriteExt].
///
/// Accepts anything implementing AsRef<[u8]> as payload.
///
/// See [read_bytes] for a description of the format.
///
/// Note: if performance matters to you, make sure your
/// [AsyncWriteExt] handle is buffered. This function is quite
/// write-intesive.
pub async fn write_bytes<W: AsyncWriteExt + Unpin, B: AsRef<[u8]>>(
    w: &mut W,
    b: B,
) -> io::Result<()> {
    // write the size packet.
    w.write_u64_le(b.as_ref().len() as u64).await?;

    // write the payload
    w.write_all(b.as_ref()).await?;

    // write padding if needed
    let padding_len = padding_len(b.as_ref().len() as u64) as usize;
    if padding_len != 0 {
        w.write_all(&EMPTY_BYTES[..padding_len]).await?;
    }
    Ok(())
}

/// Computes the number of bytes we should add to len (a length in
/// bytes) to be aligned on 64 bits (8 bytes).
pub(crate) fn padding_len(len: u64) -> u8 {
    let aligned = len.wrapping_add(7) & !7;
    aligned.wrapping_sub(len) as u8
}

#[cfg(test)]
mod tests {
    use tokio_test::{assert_ok, io::Builder};

    use super::*;
    use hex_literal::hex;

    /// The maximum length of bytes packets we're willing to accept in the test
    /// cases.
    const MAX_LEN: usize = 1024;

    #[tokio::test]
    async fn test_read_8_bytes() {
        let mut mock = Builder::new()
            .read(&8u64.to_le_bytes())
            .read(&12345678u64.to_le_bytes())
            .build();

        assert_eq!(
            &12345678u64.to_le_bytes(),
            read_bytes(&mut mock, 0..=MAX_LEN).await.unwrap().as_slice()
        );
    }

    #[tokio::test]
    async fn test_read_9_bytes() {
        let mut mock = Builder::new()
            .read(&9u64.to_le_bytes())
            .read(&hex!("01020304050607080900000000000000"))
            .build();

        assert_eq!(
            hex!("010203040506070809"),
            read_bytes(&mut mock, 0..=MAX_LEN).await.unwrap().as_slice()
        );
    }

    #[tokio::test]
    async fn test_read_0_bytes() {
        // A empty byte packet is essentially just the 0 length field.
        // No data is read, and there's zero padding.
        let mut mock = Builder::new().read(&0u64.to_le_bytes()).build();

        assert_eq!(
            hex!(""),
            read_bytes(&mut mock, 0..=MAX_LEN).await.unwrap().as_slice()
        );
    }

    #[tokio::test]
    /// Ensure we don't read any further than the size field if the length
    /// doesn't match the range we want to accept.
    async fn test_read_reject_too_large() {
        let mut mock = Builder::new().read(&100u64.to_le_bytes()).build();

        read_bytes(&mut mock, 10..=10)
            .await
            .expect_err("expect this to fail");
    }

    #[tokio::test]
    async fn test_write_bytes_no_padding() {
        let input = hex!("6478696f34657661");
        let len = input.len() as u64;
        let mut mock = Builder::new()
            .write(&len.to_le_bytes())
            .write(&input)
            .build();
        assert_ok!(write_bytes(&mut mock, &input).await)
    }
    #[tokio::test]
    async fn test_write_bytes_with_padding() {
        let input = hex!("322e332e3137");
        let len = input.len() as u64;
        let mut mock = Builder::new()
            .write(&len.to_le_bytes())
            .write(&hex!("322e332e31370000"))
            .build();
        assert_ok!(write_bytes(&mut mock, &input).await)
    }

    #[tokio::test]
    async fn test_write_string() {
        let input = "Hello, World!";
        let len = input.len() as u64;
        let mut mock = Builder::new()
            .write(&len.to_le_bytes())
            .write(&hex!("48656c6c6f2c20576f726c6421000000"))
            .build();
        assert_ok!(write_bytes(&mut mock, &input).await)
    }

    #[test]
    fn padding_len_u64_max() {
        assert_eq!(padding_len(u64::MAX), 1);
    }
}
