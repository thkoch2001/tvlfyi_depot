use pin_project_lite::pin_project;
use std::{
    ops::RangeBounds,
    task::{ready, Poll},
};
use tokio::io::AsyncRead;

use crate::wire::bytes::padding_len;

use super::bytes_writer::{BytesPacketPosition, LEN_SIZE};

pin_project! {
    /// Reads a "bytes wire packet" from the underlying reader.
    /// The format is the same as in [crate::wire::bytes::read_bytes],
    /// however this structure provides a [AsyncRead] interface,
    /// allowing to not having to pass around the entire payload in memory.
    ///
    /// After being constructed with the underlying reader and an allowed size,
    /// subsequent requests to poll_read will return payload data until the end
    /// of the packet is reached.
    ///
    /// Internally, it will first read over the size packet, filling read_size,
    /// ensuring it fits allowed_size, then return payload data.
    /// It will only signal EOF (returning `Ok(())` without filling the buffer)
    /// when all padding has been successfully consumed too.
    ///
    /// In case of an error due to size constraints, or in case of not reading
    /// all the way to the end, the underlying reader is no longer usable and
    /// might return garbage.
    pub struct BytesReader<R, S>
    where
    R: AsyncRead,
    S: RangeBounds<u64>,

    {
        #[pin]
        inner: R,

        allowed_size: S,
        read_size: [u8; 8],
        state: BytesPacketPosition,
    }
}

impl<R, S> BytesReader<R, S>
where
    R: AsyncRead + Unpin,
    S: RangeBounds<u64>,
{
    /// Constructs a new BytesReader, using the underlying passed reader.
    pub fn new(r: R, allowed_size: S) -> Self {
        Self {
            inner: r,
            allowed_size,
            read_size: [0; 8],
            state: BytesPacketPosition::Size(0),
        }
    }
}
/// Returns an error if the passed usize is 0.
fn ensure_nonzero_bytes_read(bytes_read: usize) -> Result<usize, std::io::Error> {
    if bytes_read == 0 {
        Err(std::io::Error::new(
            std::io::ErrorKind::UnexpectedEof,
            "underlying reader returned EOF",
        ))
    } else {
        Ok(bytes_read)
    }
}

impl<R, S> AsyncRead for BytesReader<R, S>
where
    R: AsyncRead,
    S: RangeBounds<u64>,
{
    fn poll_read(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        buf: &mut tokio::io::ReadBuf<'_>,
    ) -> Poll<std::io::Result<()>> {
        let mut this = self.project();

        // Use a loop, so we can deal with (multiple) state transitions.
        loop {
            match **dbg!(&this.state) {
                BytesPacketPosition::Size(LEN_SIZE) => {
                    // used in case an invalid size was signalled.
                    Err(std::io::Error::new(
                        std::io::ErrorKind::InvalidData,
                        "signalled package size not in allowed range",
                    ))?
                }
                BytesPacketPosition::Size(pos) => {
                    // try to read more of the size field.
                    // We wrap a BufRead around this.read_size here, and set_filled.
                    let mut read_buf = tokio::io::ReadBuf::new(this.read_size);
                    read_buf.set_filled(pos);
                    ready!(this.inner.as_mut().poll_read(cx, &mut read_buf))?;

                    ensure_nonzero_bytes_read(read_buf.filled().len() - pos)?;

                    let total_size_read = read_buf.filled().len();
                    if total_size_read == LEN_SIZE {
                        // Parse signalled length, validate it is allowed, and
                        // only then transition to read the payload (or padding,
                        // if the payload is empty).
                        // Otherwise, transition to failure mode
                        // `BytesPacketPosition::Size(LEN_SIZE)`, where only an error is returned.
                        let signalled_size = u64::from_le_bytes(*this.read_size);
                        if !this.allowed_size.contains(&signalled_size) {
                            *this.state = BytesPacketPosition::Size(LEN_SIZE)
                        } else if signalled_size == 0 {
                            *this.state = BytesPacketPosition::Padding(0)
                        } else {
                            *this.state = BytesPacketPosition::Payload(0)
                        }
                    } else {
                        *this.state = BytesPacketPosition::Size(total_size_read)
                    }
                }
                BytesPacketPosition::Payload(pos) => {
                    let signalled_size = u64::from_le_bytes(*this.read_size);
                    // We don't enter this match arm at all if we're expecting empty payload
                    debug_assert!(signalled_size > 0, "signalled size must be larger than 0");

                    // The passed buffer might already contain some filled data,
                    // so we need to keep track of that number before filling it further.
                    let bytes_read_before = buf.filled().len();

                    // read from the underlying reader.
                    // We cap the ReadBuf to the size of the payload, as we
                    // don't want to leak padding to the caller.
                    // Reducing these two u64 to usize on 32bits is fine - we
                    // only care about not reading too much, not too less.
                    let mut limited_buf = buf.take((signalled_size - pos) as usize);
                    ready!(this.inner.as_mut().poll_read(cx, &mut limited_buf))?;
                    let bytes_read = limited_buf.filled().len();
                    drop(limited_buf);

                    // SAFETY: we just did populate this, but through limited_buf.
                    unsafe { buf.assume_init(bytes_read) }
                    buf.set_filled(bytes_read_before + bytes_read);

                    ensure_nonzero_bytes_read(bytes_read)?;

                    if pos + bytes_read as u64 == signalled_size {
                        // if we read all payload, transition to padding state (and give it a chance to read the padding).
                        *this.state = BytesPacketPosition::Padding(0);
                    } else {
                        // if we didn't read everything yet, return what we got so far.
                        *this.state = BytesPacketPosition::Payload(pos + bytes_read as u64);
                        return Ok(()).into();
                    }
                }
                BytesPacketPosition::Padding(pos) => {
                    // Consume whatever padding is left, ensuring it's null bytes.
                    // Only signal EOF once we're past the padding.

                    let signalled_size = u64::from_le_bytes(*this.read_size);
                    let total_padding_len = padding_len(signalled_size) as usize;

                    let padding_len_left = total_padding_len - pos;
                    if padding_len_left != 0 {
                        // create a buffer only accepting the number of remaining padding bytes.
                        let mut buf = [0; 8];
                        let mut padding_buf = tokio::io::ReadBuf::new(&mut buf);
                        let mut padding_buf = padding_buf.take(total_padding_len - pos);

                        // read into padding_buf.
                        ready!(this.inner.as_mut().poll_read(cx, &mut padding_buf))?;
                        let bytes_read = dbg!(padding_buf.filled().len());

                        dbg!(ensure_nonzero_bytes_read(bytes_read))?;

                        dbg!(&padding_buf.filled());

                        *this.state = BytesPacketPosition::Padding(pos + bytes_read);

                        // ensure the bytes are not null bytes
                        if !padding_buf.filled().iter().all(|e| *e == b'\0') {
                            eprintln!("UUUHHH OOOH");
                            return Err(std::io::Error::new(
                                std::io::ErrorKind::InvalidData,
                                "padding is not all zeroes",
                            ))
                            .into();
                        }

                        // if we still have padding to read, run the loop again.
                        continue;
                    }
                    // return EOF
                    return Ok(()).into();
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::wire::bytes::write_bytes;
    use hex_literal::hex;
    use lazy_static::lazy_static;
    use tokio::io::AsyncReadExt;
    use tokio_test::{assert_err, io::Builder};

    use super::*;

    /// The maximum length of bytes packets we're willing to accept in the test
    /// cases.
    const MAX_LEN: u64 = 1024;

    lazy_static! {
        pub static ref LARGE_PAYLOAD: Vec<u8> = (0..255).collect::<Vec<u8>>().repeat(4 * 1024);
    }

    /// Helper function, calling the (simpler) write_bytes with the payload.
    /// We use this to create data we want to read from the wire.
    async fn produce_packet_bytes(payload: &[u8]) -> Vec<u8> {
        let mut exp = vec![];
        write_bytes(&mut exp, payload).await.unwrap();
        exp
    }

    /// Read an empty bytes packet.
    #[tokio::test]
    async fn read_empty() {
        let payload = &[];
        let mut mock = Builder::new()
            .read(&produce_packet_bytes(payload).await)
            .build();

        let mut r = BytesReader::new(&mut mock, ..MAX_LEN);
        let mut buf = Vec::new();
        r.read_to_end(&mut buf).await.expect("must succeed");

        assert_eq!(payload, &buf[..]);
    }

    /// Read a 1 bytes payload packet
    #[tokio::test]
    async fn read_1b() {
        let payload = &[0xff];
        let mut mock = Builder::new()
            .read(&produce_packet_bytes(payload).await)
            .build();

        let mut r = BytesReader::new(&mut mock, ..MAX_LEN);
        let mut buf = Vec::new();
        r.read_to_end(&mut buf).await.expect("must succeed");

        assert_eq!(payload, &buf[..]);
    }

    /// Read a 8 bytes payload packet (no padding)
    #[tokio::test]
    async fn read_8b() {
        let payload = &hex!("0001020304050607");
        let mut mock = Builder::new()
            .read(&produce_packet_bytes(payload).await)
            .build();

        let mut r = BytesReader::new(&mut mock, ..MAX_LEN);
        let mut buf = Vec::new();
        r.read_to_end(&mut buf).await.expect("must succeed");

        assert_eq!(payload, &buf[..]);
    }

    /// Read a larger bytes packet
    #[tokio::test]
    async fn read_1m() {
        let payload = LARGE_PAYLOAD.as_slice();
        let mut mock = Builder::new()
            .read(&produce_packet_bytes(payload).await)
            .build();

        let mut r = BytesReader::new(&mut mock, ..(LARGE_PAYLOAD.len() as u64) + 1);
        let mut buf = Vec::new();
        r.read_to_end(&mut buf).await.expect("must succeed");

        assert_eq!(payload, &buf[..]);
    }

    /// Fail if the bytes packet is larger than allowed
    #[tokio::test]
    async fn read_bigger_than_allowed_fail() {
        let payload = LARGE_PAYLOAD.as_slice();
        let mut mock = Builder::new()
            .read(&produce_packet_bytes(payload).await[0..8]) // We stop reading after the size packet
            .build();

        let mut r = BytesReader::new(&mut mock, ..2048);
        let mut buf = Vec::new();
        assert_err!(r.read_to_end(&mut buf).await);
    }

    /// Fail if the bytes packet is smaller than allowed
    #[tokio::test]
    async fn read_smaller_than_allowed_fail() {
        let payload = &[0x00, 0x01, 0x02];
        let mut mock = Builder::new()
            .read(&produce_packet_bytes(payload).await[0..8]) // We stop reading after the size packet
            .build();

        let mut r = BytesReader::new(&mut mock, 1024..2048);
        let mut buf = Vec::new();
        assert_err!(r.read_to_end(&mut buf).await);
    }

    /// Fail if the padding is not all zero
    #[tokio::test]
    async fn read_fail_if_nonzero_padding() {
        let payload = &[0x00, 0x01, 0x02];
        let mut packet_bytes = produce_packet_bytes(payload).await;
        // Flip some bits in the padding
        packet_bytes[12] = 0xff;
        dbg!(&packet_bytes);
        let mut mock = Builder::new().read(&packet_bytes).build(); // We stop reading after the faulty bit

        let mut r = BytesReader::new(&mut mock, ..MAX_LEN);
        let mut buf = Vec::new();

        // FIXME: this throws a debug_assert inside tokios read_to_end impl itself.
        // Should it?
        r.read_to_end(&mut buf).await.expect_err("must fail");
    }

    // TODO
    // - EOF scenarios
    // - other errors from the underlying reader
}
