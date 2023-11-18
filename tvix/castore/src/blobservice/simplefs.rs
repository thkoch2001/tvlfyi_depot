use std::{
    path::{Path, PathBuf},
    pin::pin,
    task::Poll,
};

use bytes::Buf;
use data_encoding::HEXLOWER;
use pin_project_lite::pin_project;
use tokio::io::AsyncWriteExt;
use tonic::async_trait;
use tracing::{debug, error, instrument};

use crate::{B3Digest, Error};

use super::{BlobReader, BlobService, BlobWriter};

#[derive(Clone)]
pub struct SimpleFilesystemBlobService {
    /// Where the blobs are located on a filesystem already mounted.
    path: PathBuf,
}

impl SimpleFilesystemBlobService {
    pub async fn new(path: PathBuf) -> std::io::Result<Self> {
        tokio::fs::create_dir_all(&path).await?;
        tokio::fs::create_dir_all(path.join("tmp")).await?;

        Ok(Self { path })
    }
}

fn derive_path(root: &Path, digest: &B3Digest) -> PathBuf {
    let prefix = HEXLOWER.encode(&digest.as_slice()[..2]);
    let pathname = HEXLOWER.encode(digest.as_slice());

    root.join(prefix).join(pathname)
}

#[async_trait]
impl BlobService for SimpleFilesystemBlobService {
    #[instrument(skip_all, ret, err, fields(blob.digest=%digest))]
    async fn has(&self, digest: &B3Digest) -> Result<bool, Error> {
        Ok(tokio::fs::try_exists(derive_path(&self.path, digest)).await?)
    }

    #[instrument(skip_all, err, fields(blob.digest=%digest))]
    async fn open_read(&self, digest: &B3Digest) -> Result<Option<Box<dyn BlobReader>>, Error> {
        let dst_path = derive_path(&self.path, digest);
        let reader = match tokio::fs::File::open(dst_path).await {
            Ok(file) => {
                debug!("{:?}", file);
                let reader: Box<dyn BlobReader> = Box::new(file);
                Ok(Some(reader))
            }
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => Ok(None),
            Err(e) => {
                error!(e=%e, "failed to open file");
                Err(e)
            }
        };

        Ok(reader?)
    }

    #[instrument(skip_all)]
    async fn open_write(&self) -> Box<dyn BlobWriter> {
        let file = match async_tempfile::TempFile::new_in(self.path.join("tmp")).await {
            Ok(file) => Ok(file),
            Err(e) => match e {
                async_tempfile::Error::Io(io_error) => Err(io_error),
                async_tempfile::Error::InvalidFile => Err(std::io::Error::new(
                    std::io::ErrorKind::NotFound,
                    "invalid or missing file specified",
                )),
                async_tempfile::Error::InvalidDirectory => Err(std::io::Error::new(
                    std::io::ErrorKind::NotFound,
                    "invalid or missing directory specified",
                )),
            },
        };

        Box::new(SimpleFilesystemBlobWriter {
            root: self.path.clone(),
            file,
            digester: blake3::Hasher::new(),
        })
    }
}

pin_project! {
    struct SimpleFilesystemBlobWriter {
        root: PathBuf,
        file: std::io::Result<async_tempfile::TempFile>,
        digester: blake3::Hasher
    }
}

impl tokio::io::AsyncWrite for SimpleFilesystemBlobWriter {
    fn poll_write(
        mut self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        buf: &[u8],
    ) -> std::task::Poll<Result<usize, std::io::Error>> {
        if let Err(e) = self.file.as_mut() {
            return Poll::Ready(Err(std::mem::replace(
                e,
                std::io::Error::new(
                    std::io::ErrorKind::NotConnected,
                    "this file is already closed",
                ),
            )));
        }

        let writer = self.file.as_mut().unwrap();
        match pin!(writer).poll_write(cx, buf) {
            Poll::Ready(Ok(n)) => {
                let this = self.project();
                this.digester.update(buf.take(n).into_inner());
                Poll::Ready(Ok(n))
            }
            Poll::Ready(Err(e)) => Poll::Ready(Err(e)),
            Poll::Pending => Poll::Pending,
        }
    }

    fn poll_flush(
        mut self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Result<(), std::io::Error>> {
        if let Err(e) = self.file.as_mut() {
            return Poll::Ready(Err(std::mem::replace(
                e,
                std::io::Error::new(
                    std::io::ErrorKind::NotConnected,
                    "this file is already closed",
                ),
            )));
        }

        let writer = self.file.as_mut().unwrap();
        pin!(writer).poll_flush(cx)
    }

    fn poll_shutdown(
        mut self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Result<(), std::io::Error>> {
        if let Err(e) = self.file.as_mut() {
            return Poll::Ready(Err(std::mem::replace(
                e,
                std::io::Error::new(
                    std::io::ErrorKind::NotConnected,
                    "this file is already closed",
                ),
            )));
        }

        let writer = self.file.as_mut().unwrap();
        pin!(writer).poll_shutdown(cx)
    }
}

#[async_trait]
impl BlobWriter for SimpleFilesystemBlobWriter {
    async fn close(&mut self) -> Result<B3Digest, Error> {
        if let Err(e) = self.file.as_mut() {
            return Err(std::mem::replace(
                e,
                std::io::Error::new(
                    std::io::ErrorKind::NotConnected,
                    "this file is already closed",
                ),
            )
            .into());
        }

        let writer = self.file.as_mut().unwrap();
        writer.sync_all().await?;
        writer.flush().await?;

        let digest: B3Digest = self.digester.finalize().as_bytes().into();
        let dst_path = derive_path(&self.root, &digest);
        tokio::fs::create_dir_all(dst_path.parent().unwrap()).await?;
        tokio::fs::rename(writer.file_path(), dst_path).await?;

        Ok(digest)
    }
}
