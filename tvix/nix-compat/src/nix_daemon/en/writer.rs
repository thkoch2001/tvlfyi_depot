use bytes::Bytes;
use pin_project_lite::pin_project;
use tokio::io::AsyncWriteExt;

use crate::ProtocolVersion;

use super::NixWrite;

pin_project! {
pub struct NixWriter<W> {
    #[pin]
    inner: W,
    version: ProtocolVersion
}
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
    W: AsyncWriteExt + Send + Unpin + Sync,
{
    type Error = std::io::Error;

    fn version(&self) -> ProtocolVersion {
        self.version
    }

    async fn write_number(&mut self, value: u64) -> Result<(), Self::Error> {
        self.inner.write_u64_le(value).await
    }

    async fn write_bytes(&mut self, mut value: bytes::Bytes) -> Result<(), Self::Error> {
        let len = value.len();
        let pad = (8 - (len % 8)) % 8;
        self.write_number(len as u64).await?;
        self.inner.write_all_buf(&mut value).await?;
        self.inner
            .write_all_buf(&mut Bytes::from(vec![0; pad]))
            .await
    }

    async fn write<V: super::NixSerialize + Send>(&mut self, value: V) -> Result<(), Self::Error> {
        value.serialize(self).await
    }
}
