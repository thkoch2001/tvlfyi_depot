use pin_project_lite::pin_project;
use std::task::Poll;

use tokio::io::AsyncWrite;

/// The length of the size field, in bytes is always 8.
const LEN_SIZE: u8 = 8;

/// 8 null bytes, used to write out padding.
const EMPTY_BYTES: &[u8; 8] = &[0u8; 8];

pin_project! {
    /// Writes a byte packet to the underlying writer.
    /// The user of this Writer needs to specify the expected payload size upfront,
    /// can then write to it.
    ///
    /// At the end, the user MUST call flush, which will validate the payload size
    /// written to match, and write the necessary padding.
    ///
    /// In case flush is not called, invalid data might be sent silently.
    ///
    /// FUTUREWORK: handle flush/shutdown for empty fields.
    ///
    /// The struct holds three fields, the underlying writer, the (expected)
    /// payload length, and an enum, tracking the state.
    pub struct BytesWriter<W>
    where
        W: AsyncWrite,
    {
        #[pin]
        inner: W,
        payload_len: u64,
        state: BytesWriterState,
    }
}

/// Models the state [BytesWriter] currently is in.
/// It can be in three stages, writing size, payload or padding.
/// The numbers track the number of bytes written in the current state.
/// There shall be no ambiguous states, at the end of a stage we immediately
/// move to the beginning of the next one:
/// - WriteSize(LEN_SIZE) must be expressed as WritePayload(0)
/// - WritePayload(self.payload_len) must be expressed as WritePadding(0)
///
/// WritePadding(padding_len) means everything that needed to be written was
/// written.
#[derive(Clone, Debug, PartialEq, Eq)]
enum BytesWriterState {
    WriteSize(usize),
    WritePayload(u64),
    WritePadding(usize),
}

impl<W> BytesWriter<W>
where
    W: AsyncWrite,
{
    /// Constructs a new BytesWriter, using the underlying passed writer.
    pub fn new(w: W, payload_len: u64) -> Self {
        Self {
            inner: w,
            payload_len,
            state: BytesWriterState::WriteSize(0),
        }
    }
}

impl<W> AsyncWrite for BytesWriter<W>
where
    W: AsyncWrite,
{
    fn poll_write(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        buf: &[u8],
    ) -> Poll<Result<usize, std::io::Error>> {
        dbg!("poll_write with buf", buf.len());
        // We can't use a loop here to deal with (multiple) state transitions,
        // as we can only call poll_write() once.
        let this = self.project();
        match **dbg!(&this.state) {
            // If we're still busy writing the size field, try to write
            // that, and arrange to be woken up again.
            BytesWriterState::WriteSize(pos) => {
                let size_field = &this.payload_len.to_le_bytes()[..];

                if let Poll::Ready(resp) = this.inner.poll_write(cx, &size_field[pos..]) {
                    let new_pos = pos + resp?;
                    if new_pos == LEN_SIZE as usize {
                        *this.state = BytesWriterState::WritePayload(0);
                    } else {
                        *this.state = BytesWriterState::WriteSize(new_pos);
                    }
                }
                dbg!("new state", &this.state);

                // arrange to be immediately woken up again
                cx.waker().wake_by_ref();
                return Poll::Pending;
            }
            // Try to write as much payload as possible.
            BytesWriterState::WritePayload(pos) => match this.inner.poll_write(cx, buf) {
                Poll::Ready(resp) => {
                    let bytes_written = resp?;
                    let new_pos = pos + (bytes_written as u64);
                    if new_pos == *this.payload_len {
                        dbg!("done with payload", &new_pos);
                        *this.state = BytesWriterState::WritePadding(0)
                    } else {
                        dbg!("still payload", new_pos);
                        *this.state = BytesWriterState::WritePayload(new_pos)
                    }

                    dbg!("new state", &this.state);

                    return Poll::Ready(Ok(bytes_written));
                }
                Poll::Pending => return Poll::Pending,
            },
            // If we're already in padding state, there should be no more payload left to write!
            BytesWriterState::WritePadding(_pos) => {
                return Poll::Ready(Err(std::io::Error::new(
                    std::io::ErrorKind::BrokenPipe,
                    "tried to write excess bytes",
                )));
            }
        }
    }

    fn poll_flush(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> Poll<Result<(), std::io::Error>> {
        dbg!("flush called");
        let this = self.project();
        match *this.state {
            BytesWriterState::WriteSize(8) => unreachable!(),
            // More bytes to write in the size field
            BytesWriterState::WriteSize(pos) => {
                let size_field = &this.payload_len.to_le_bytes()[..];
                if let Poll::Ready(resp) = this.inner.poll_write(cx, &size_field[pos..]) {
                    let new_pos = pos + resp?;
                    if new_pos == LEN_SIZE as usize {
                        // We wrote all bytes of the size field and are now ready to receive payload
                        *this.state = BytesWriterState::WritePayload(0);
                        return Poll::Ready(Ok(()));
                    } else {
                        *this.state = BytesWriterState::WriteSize(new_pos);
                    }
                }

                // arrange to be immediately woken up again
                cx.waker().wake_by_ref();
                return Poll::Pending;
            }
            BytesWriterState::WritePayload(_pos) => {
                // We don't have any internal payload buffer, so flushing is a no-op.
                Poll::Ready(Ok(()))
            }
            BytesWriterState::WritePadding(pos) => {
                // Write remaining padding.
                let padding_len = super::bytes::padding_len(*this.payload_len) as usize;
                if pos == padding_len {
                    // already wrote all of padding
                    return Poll::Ready(Ok(()));
                }

                if let Poll::Ready(resp) = this.inner.poll_write(cx, &EMPTY_BYTES[..padding_len]) {
                    let new_pos = pos + resp?;
                    *this.state = BytesWriterState::WritePadding(new_pos);

                    if new_pos == padding_len as usize {
                        return Poll::Ready(Ok(()));
                    }
                }

                // arrange to be immediately woken up again
                cx.waker().wake_by_ref();
                Poll::Pending
            }
        }
    }

    fn poll_shutdown(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> Poll<Result<(), std::io::Error>> {
        dbg!("shutdown called");
        let this = self.project();
        match **dbg!(&this.state) {
            // If we're in BytesWriterState::WritePadding, call flush (which will
            // write out remaining padding).
            BytesWriterState::WritePadding(pos) => {
                let padding_len = super::bytes::padding_len(*this.payload_len) as usize;

                if pos == padding_len {
                    // already at the end, close the underlying writer.
                    return this.inner.poll_shutdown(cx);
                }

                // Write remaining padding.
                if let Poll::Ready(resp) = this.inner.poll_write(cx, &EMPTY_BYTES[..padding_len]) {
                    let new_pos = pos + resp?;
                    *this.state = BytesWriterState::WritePadding(new_pos);
                    // We arrange an additional call, and let that one call poll_shutdown.
                }
                cx.waker().wake_by_ref();
                return Poll::Pending;
            }
            // In all other cases, we can't really do a clean shutdown, other
            // than closing the connection - so just do that.
            // However, even in the successful shutdown of the underlying writer
            // we propagate the error.
            BytesWriterState::WriteSize(_) | BytesWriterState::WritePayload(_) => {
                match this.inner.poll_shutdown(cx) {
                    Poll::Ready(Err(e)) => Poll::Ready(Err(e)),
                    Poll::Ready(Ok(())) => Poll::Ready(Err(std::io::Error::new(
                        std::io::ErrorKind::BrokenPipe,
                        "unclean shutdown",
                    ))),
                    Poll::Pending => {
                        cx.waker().wake_by_ref();
                        Poll::Pending
                    }
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
    use tokio::io::AsyncWriteExt;
    use tokio_test::{assert_err, assert_ok, io::Builder};

    use super::*;

    lazy_static! {
        pub static ref LARGE_PAYLOAD: Vec<u8> = (0..255).collect::<Vec<u8>>().repeat(4 * 1024);
    }

    /// Helper function, calling the (simpler) write_bytes with the payload.
    /// We use this to create data we want to see on the wire.
    async fn produce_exp_bytes(payload: &[u8]) -> Vec<u8> {
        let mut exp = vec![];
        write_bytes(&mut exp, payload).await.unwrap();
        exp
    }

    /// Write an empty bytes packet.
    #[tokio::test]
    async fn test_write_empty() {
        let payload = &[];
        let mut mock = Builder::new()
            .write(&produce_exp_bytes(payload).await)
            .build();

        let mut w = BytesWriter::new(&mut mock, 0);
        assert_ok!(w.write_all(&[]).await, "write all data");
        assert_ok!(w.flush().await, "flush");
    }

    /// Write an empty bytes packet, not calling write.
    #[tokio::test]
    async fn test_write_empty_only_flush() {
        let payload = &[];
        let mut mock = Builder::new()
            .write(&produce_exp_bytes(payload).await)
            .build();

        let mut w = BytesWriter::new(&mut mock, 0);
        assert_ok!(w.flush().await, "flush");
    }

    /// Write a 1 bytes packet
    #[tokio::test]
    async fn test_write_1b() {
        let payload = &[0xff];

        let mut mock = Builder::new()
            .write(&produce_exp_bytes(payload).await)
            .build();

        let mut w = BytesWriter::new(&mut mock, payload.len() as u64);
        assert_ok!(w.write_all(payload).await);
        assert_ok!(w.flush().await, "flush");
    }

    /// Write a 8 bytes payload (no padding)
    #[tokio::test]
    async fn test_write_8b() {
        let payload = &hex!("000102030405060708");

        let mut mock = Builder::new()
            .write(&produce_exp_bytes(payload).await)
            .build();

        let mut w = BytesWriter::new(&mut mock, payload.len() as u64);
        assert_ok!(w.write_all(payload).await);
        assert_ok!(w.flush().await, "flush");
    }

    /// Write a 9 bytes payload (7 bytes padding)
    #[tokio::test]
    async fn test_write_9b() {
        let payload = &hex!("00010203040506070809");

        let mut mock = Builder::new()
            .write(&produce_exp_bytes(payload).await)
            .build();

        let mut w = BytesWriter::new(&mut mock, payload.len() as u64);
        assert_ok!(w.write_all(payload).await);
        assert_ok!(w.flush().await, "flush");
    }

    /// Write a 9 bytes packet very granularly, with a lot of flushing in between,
    /// and a shutdown at the end.
    #[tokio::test]
    async fn test_write_9b_flush() {
        let payload = &hex!("00010203040506070809");
        let exp_bytes = produce_exp_bytes(payload).await;

        let mut mock = Builder::new().write(&exp_bytes).build();

        let mut w = BytesWriter::new(&mut mock, payload.len() as u64);
        assert_ok!(w.flush().await);

        assert_ok!(w.write_all(&payload[..4]).await);
        assert_ok!(w.flush().await);

        // empty write, cause why not
        assert_ok!(w.write_all(&[]).await);
        assert_ok!(w.flush().await);

        assert_ok!(w.write_all(&payload[4..]).await);
        assert_ok!(w.flush().await);
        assert_ok!(w.shutdown().await);
    }

    /// Write a larger bytes packet
    #[tokio::test]
    async fn test_write_1m_debug() {
        let payload = LARGE_PAYLOAD.as_slice();
        let exp_bytes = produce_exp_bytes(payload).await;

        let mut mock = Builder::new().write(&exp_bytes).build();
        let mut w = BytesWriter::new(&mut mock, payload.len() as u64);

        assert_ok!(w.write_all(payload).await);
        assert_ok!(w.flush().await, "flush");
    }

    /// Not calling flush at the end, but shutdown is also ok if we wrote all
    /// bytes we promised to write (as shutdown implies flush)
    #[tokio::test]
    async fn test_write_shutdown_without_flush_end() {
        let payload = &[0xf0, 0xff];
        let exp_bytes = produce_exp_bytes(payload).await;

        let mut mock = Builder::new().write(&exp_bytes).build();
        let mut w = BytesWriter::new(&mut mock, payload.len() as u64);

        // call flush to write the size field
        assert_ok!(w.flush().await);

        // write payload
        assert_ok!(w.write_all(payload).await);

        // call shutdown
        assert_ok!(w.shutdown().await);
    }

    /// Calling shutdown while not having written all bytes that were promised
    /// returns an error.
    /// Note there's still cases of silent corruption if the user doesn't call
    /// shutdown explicitly (only drops).
    #[tokio::test]
    async fn test_premature_shutdown() {
        let payload = &[0xf0, 0xff];
        let mut buf = Vec::new();
        let mut w = BytesWriter::new(&mut buf, payload.len() as u64);

        // call flush to write the size field
        assert_ok!(w.flush().await);

        // write half of the payload (!)
        assert_ok!(w.write_all(&payload[0..1]).await);

        // call shutdown, ensure it fails
        assert_err!(w.shutdown().await);
    }

    // TODO: add faulty writer, ensure write errors are propagated.
}
