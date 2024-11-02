use tokio::io::AsyncWriteExt;

use crate::ProtocolVersion;

use super::{NixSerialize, NixWrite};

pub struct NixWriter<W> {
    inner: W,
    version: ProtocolVersion,
}

impl<W> NixWriter<W>
where
    W: AsyncWriteExt,
{
    pub fn new(writer: W, version: ProtocolVersion) -> Self {
        Self {
            inner: writer,
            version,
        }
    }
}

impl<W> NixWrite for NixWriter<W>
where
    W: AsyncWriteExt + Unpin + Send,
{
    type Error = std::io::Error;

    fn version(&self) -> ProtocolVersion {
        self.version
    }

    async fn write_number(&mut self, value: u64) -> Result<(), Self::Error> {
        self.inner.write_u64_le(value).await
    }

    async fn write_bytes<'a>(&'a mut self, value: &'a [u8]) -> Result<(), Self::Error> {
        self.write_number(value.len() as u64).await?;
        self.inner.write_all(value).await?;
        let pad = (8 - (value.len() % 8)) % 8;
        for _ in 0..pad {
            println!("pad: {}", pad);
            self.inner.write_u8(0).await?;
        }
        Ok(())
    }

    async fn write<'a, T: NixSerialize + Sync>(
        &'a mut self,
        value: &'a T,
    ) -> Result<(), Self::Error> {
        value.serialize(self).await
    }
}
