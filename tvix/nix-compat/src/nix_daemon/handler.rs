use std::{future::Future, sync::Arc};

use thiserror::Error;
use tokio::{
    io::{split, AsyncReadExt, AsyncWriteExt, ReadHalf, WriteHalf},
    sync::Mutex,
};
use tracing::debug;

use super::{
    de::NixReader,
    ser::{NixSerialize, NixWriter, NixWriterBuilder},
    worker_protocol::{server_handshake_client, ClientSettings, Operation, Trust, STDERR_LAST},
    NixDaemonIO, ProtocolVersion,
};
use crate::nix_daemon::de::NixRead;
use crate::{
    nix_daemon::{ser::NixWrite, types::NixError},
    worker_protocol::STDERR_ERROR,
};

/// Errors that can happen as part of handling nix daemon client connection.
#[derive(Error, Debug)]
pub enum NixDaemonError {
    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),
    
    #[error("Operation `{0:?}` is not implemented yet.")]
    OperationNotImplemented(Operation),
    
    #[error("Unknown operation opcode `{0}`.")]
    UnknownOperation(u64),
}

#[allow(dead_code)]
pub struct NixDaemon<IO, R, W> {
    io: Arc<IO>,
    protocol_version: ProtocolVersion,
    client_settings: ClientSettings,
    reader: NixReader<R>,
    writer: Arc<Mutex<NixWriter<W>>>,
}

impl<IO, R, W> NixDaemon<IO, R, W>
where
    IO: NixDaemonIO + Sync + Send,
{
    pub fn new(
        io: Arc<IO>,
        protocol_version: ProtocolVersion,
        client_settings: ClientSettings,
        reader: NixReader<R>,
        writer: NixWriter<W>,
    ) -> Self {
        Self {
            io,
            protocol_version,
            client_settings,
            reader,
            writer: Arc::new(Mutex::new(writer)),
        }
    }
}

impl<IO, RW> NixDaemon<IO, ReadHalf<RW>, WriteHalf<RW>>
where
    RW: AsyncReadExt + AsyncWriteExt + Send + Unpin + 'static,
    IO: NixDaemonIO + Sync + Send,
{
    /// Async constructor for NixDaemon.
    ///
    /// Performs the initial handshake with the client and retrieves the client's preferred
    /// settings.
    ///
    /// The resulting daemon can handle the client session by calling `handle_client()`.
    pub async fn initialize(io: Arc<IO>, mut connection: RW) -> Result<Self, std::io::Error>
    where
        RW: AsyncReadExt + AsyncWriteExt + Send + Unpin,
    {
        let protocol_version =
            server_handshake_client(&mut connection, "2.18.2", Trust::Trusted).await?;

        connection.write_u64_le(STDERR_LAST).await?;
        let (reader, writer) = split(connection);
        let mut reader = NixReader::builder()
            .set_version(protocol_version)
            .build(reader);
        let mut writer = NixWriterBuilder::default()
            .set_version(protocol_version)
            .build(writer);

        // The first op is always SetOptions
        let operation: Operation = reader.read_value().await?;
        if operation != Operation::SetOptions {
            return Err(std::io::Error::other(
                "Expected SetOptions operation, but got {operation}",
            ));
        }
        let client_settings: ClientSettings = reader.read_value().await?;
        writer.write_number(STDERR_LAST).await?;
        writer.flush().await?;

        Ok(Self::new(
            io,
            protocol_version,
            client_settings,
            reader,
            writer,
        ))
    }

    /// Main client connection loop, reads client's requests and responds to them accordingly.
    pub async fn handle_client(&mut self) -> Result<(), std::io::Error> {
        loop {
            let op_code = self.reader.read_number().await?;
            if let Ok(operation) = TryInto::<Operation>::try_into(op_code) {
                match operation {
                    Operation::SetOptions => {
                        self.client_settings = self.reader.read_value().await?;
                        self.handle(async { Ok(()) }).await?
                    }
                    _ => {
                        return Err(std::io::Error::other(
                            "Operation {operation} is not implemented",
                        ));
                    }
                }
            }
        }
    }

    /// Handles the operation and sends the response or error to the client.
    /// 
    /// As per nix daemon protocol, after sending the request, the client expects zero or more
    /// log lines/activities followed by either
    /// * STDERR_LAST and the response bytes
    /// * STDERR_ERROR and the error
    /// 
    /// This this is a helper method that handles that, it awaits on the passed in future and then
    /// handles is as described above.
    async fn handle<T>(
        &mut self,
        future: impl Future<Output = Result<T, NixDaemonError>>,
    ) -> Result<(), std::io::Error>
    where
        T: NixSerialize + Send,
    {
        let result = future.await;
        let mut writer = self.writer.lock().await;

        match result {
            Ok(r) => {
                writer.write_number(STDERR_LAST).await?;
                writer.write_value(&r).await?;
                writer.flush().await
            }
            Err(NixDaemonError::Io(e)) => {
                debug!("IO error: {e:?}");
                writer.write_number(STDERR_ERROR).await?;
                writer.write_value(&NixError::new(format!("{e:?}"))).await?;
                writer.flush().await
            }
            Err(e) => {
                return Err(std::io::Error::other(format!("{e:?}")));
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Arc;

    use tokio::io::AsyncWriteExt;

    use crate::{
        worker_protocol::{ClientSettings, WORKER_MAGIC_1, WORKER_MAGIC_2},
        ProtocolVersion,
    };

    struct MockDaemonIO {}

    impl NixDaemonIO for MockDaemonIO {}

    #[tokio::test]
    async fn test_daemon_initialization() {
        let mut builder = tokio_test::io::Builder::new();
        let test_conn = builder
            .read(&WORKER_MAGIC_1.to_le_bytes())
            .write(&WORKER_MAGIC_2.to_le_bytes())
            .write(&[37, 1, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00])
            // Let's say the client is in sync with the daemon
            // protocol-wise
            .read(&[35, 1, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00])
            // cpu affinity
            .read(&[0; 8])
            // reservespace
            .read(&[0; 8])
            // version (size)
            .write(&[0x06, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00])
            // version (data == 2.18.2 + padding)
            .write(&[50, 46, 49, 56, 46, 50, 0, 0])
            // Trusted (1 == client trusted
            .write(&[1, 0, 0, 0, 0, 0, 0, 0])
            // STDERR_LAST
            .write(&[115, 116, 108, 97, 0, 0, 0, 0]);

        let mut bytes = Vec::new();
        let mut writer = NixWriter::new(&mut bytes);
        writer
            .write_value(&ClientSettings::default())
            .await
            .unwrap();
        writer.flush().await.unwrap();

        let test_conn = test_conn
            // SetOptions op
            .read(&[19, 0, 0, 0, 0, 0, 0, 0])
            .read(&bytes)
            // STDERR_LAST
            .write(&[115, 116, 108, 97, 0, 0, 0, 0])
            .build();

        let daemon = NixDaemon::initialize(Arc::new(MockDaemonIO {}), test_conn)
            .await
            .unwrap();
        assert_eq!(daemon.client_settings, ClientSettings::default());
        assert_eq!(daemon.protocol_version, ProtocolVersion::from_parts(1, 35));

        //assert_eq!(picked_version, PROTOCOL_VERSION)
    }
}
