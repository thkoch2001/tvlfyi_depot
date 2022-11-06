use std::io::Read;

// A reader that writes everything read to a sha2::Digest.
pub struct HashReader<R: Read, D: sha2::Digest> {
    r: R,
    d: D,
    bytes_read: u32,
}

impl<R: Read, D: sha2::Digest> HashReader<R, D> {
    pub fn new(r: R, d: D) -> Self {
        HashReader {
            r,
            d,
            bytes_read: 0,
        }
    }

    pub fn digest_bytes(self) -> Vec<u8> {
        self.d.finalize().to_vec()
    }

    pub fn bytes_read(&self) -> u32 {
        self.bytes_read
    }
}

impl<R: Read, D: sha2::Digest> Read for HashReader<R, D> {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        let buf_size = buf.len() as u32;
        let result = self.r.read(buf);
        self.d.update(buf);
        self.bytes_read += buf_size;
        result
    }
}
