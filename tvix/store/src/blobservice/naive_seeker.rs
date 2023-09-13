use super::BlobReader;
use pin_project_lite::pin_project;
use std::task::Poll;
use std::{io, pin::pin};
use tokio::io::AsyncRead;
use tracing::{debug, instrument};

pin_project! {
    /// This implements [tokio::io::AsyncSeek] for and [tokio::io::AsyncRead] by
    /// simply skipping over some bytes, keeping track of the position.
    /// It fails whenever you try to seek backwards.
    pub struct NaiveSeeker<R: tokio::io::AsyncRead> {
        #[pin]
        r: tokio::io::BufReader<R>,
        pos: u64,
        bytes_to_skip: u64,
    }
}

impl<R: tokio::io::AsyncRead> NaiveSeeker<R> {
    pub fn new(r: R) -> Self {
        NaiveSeeker {
            r: tokio::io::BufReader::new(r),
            pos: 0,
            bytes_to_skip: 0,
        }
    }
}

impl<R: tokio::io::AsyncRead> tokio::io::AsyncRead for NaiveSeeker<R> {
    fn poll_read(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        buf: &mut tokio::io::ReadBuf<'_>,
    ) -> Poll<std::io::Result<()>> {
        // The amount of data read can be determined by the increase
        // in the length of the slice returned by `ReadBuf::filled`.
        let filled_before = buf.filled().len();
        let this = self.project();
        let pos: &mut u64 = this.pos;

        match this.r.poll_read(cx, buf) {
            Poll::Ready(a) => {
                let bytes_read = buf.filled().len() - filled_before;
                *pos += bytes_read as u64;

                Poll::Ready(a)
            }
            Poll::Pending => Poll::Pending,
        }
    }
}

impl<R: tokio::io::AsyncRead> tokio::io::AsyncBufRead for NaiveSeeker<R> {
    fn poll_fill_buf(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> Poll<io::Result<&[u8]>> {
        self.project().r.poll_fill_buf(cx)
    }

    fn consume(self: std::pin::Pin<&mut Self>, amt: usize) {
        let this = self.project();
        this.r.consume(amt);
        let pos: &mut u64 = this.pos;
        *pos += amt as u64;
    }
}

impl<R: tokio::io::AsyncRead> tokio::io::AsyncSeek for NaiveSeeker<R> {
    #[instrument(skip(self))]
    fn start_seek(
        self: std::pin::Pin<&mut Self>,
        position: std::io::SeekFrom,
    ) -> std::io::Result<()> {
        let absolute_offset: u64 = match position {
            io::SeekFrom::Start(start_offset) => {
                if start_offset < self.pos {
                    return Err(io::Error::new(
                        io::ErrorKind::Unsupported,
                        format!("can't seek backwards ({} -> {})", self.pos, start_offset),
                    ));
                } else {
                    start_offset
                }
            }
            // we don't know the total size, can't support this.
            io::SeekFrom::End(_end_offset) => {
                return Err(io::Error::new(
                    io::ErrorKind::Unsupported,
                    "can't seek from end",
                ));
            }
            io::SeekFrom::Current(relative_offset) => {
                if relative_offset < 0 {
                    return Err(io::Error::new(
                        io::ErrorKind::Unsupported,
                        "can't seek backwards relative to current position",
                    ));
                } else {
                    self.pos + relative_offset as u64
                }
            }
        };

        debug!(absolute_offset=?absolute_offset, "seek");

        // we already know absolute_offset is larger than self.pos
        debug_assert!(
            absolute_offset >= self.pos,
            "absolute_offset {} is larger than self.pos {}",
            absolute_offset,
            self.pos
        );

        // calculate bytes to skip
        *self.project().bytes_to_skip = absolute_offset - self.pos;

        Ok(())
    }

    #[instrument(skip(self))]
    fn poll_complete(
        mut self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> Poll<std::io::Result<u64>> {
        if self.bytes_to_skip == 0 {
            // return the new position (from the start of the stream)
            return Poll::Ready(Ok(self.pos));
        }

        // discard some bytes, until pos is where we want it to be.
        // We create a buffer that we'll discard later on.
        let mut buf = [0; 1024];

        // calculate the length we want to skip at most, which is either a max
        // buffer size, or the number of remaining bytes to read, whatever is
        // smaller.
        let bytes_to_skip = std::cmp::min(self.bytes_to_skip as usize, buf.len());

        let mut read_buf = tokio::io::ReadBuf::new(&mut buf[..bytes_to_skip]);

        loop {
            read_buf.clear();
            match self.as_mut().poll_read(cx, &mut read_buf) {
                Poll::Ready(_a) => {
                    eprintln!("ready");
                    let bytes_read = read_buf.filled().len() as u64;
                    eprintln!("bytes_read: {}", bytes_read);

                    if bytes_read == 0 {
                        return Poll::Ready(Err(io::Error::new(
                            io::ErrorKind::UnexpectedEof,
                            format!(
                                "tried to skip {} bytes, but only was able to skip {} until reaching EOF",
                                bytes_to_skip, bytes_read
                            ),
                        )));
                    }

                    // calculate bytes to skip
                    let bytes_to_skip = self.bytes_to_skip - bytes_read;
                    let pos = self.pos;
                    eprintln!("bytes_to_skip: {}, pos: {}", bytes_to_skip, pos);

                    *pin!(self.bytes_to_skip) = bytes_to_skip;

                    if bytes_to_skip == 0 {
                        return Poll::Ready(Ok(pos));
                    }
                }
                Poll::Pending => return Poll::Pending,
            };
        }
    }
}

impl<R: tokio::io::AsyncRead + Send + Unpin + 'static> BlobReader for NaiveSeeker<R> {}

#[cfg(test)]
mod tests {
    use super::NaiveSeeker;
    use std::io::{Cursor, SeekFrom};
    use tokio::io::AsyncSeekExt;

    /// This seek requires multiple `poll_read` as we use a 1024 bytes internal
    /// buffer when doing the seek.
    /// This ensures we don't hang indefinitely.
    #[tokio::test]
    async fn seek() {
        let buf = vec![0u8; 4096];
        let reader = Cursor::new(&buf);
        let mut seeker = NaiveSeeker::new(reader);
        seeker.seek(SeekFrom::Start(4000)).await.unwrap();
    }
}
