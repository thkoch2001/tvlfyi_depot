pub mod client_writer;
use std::{future::Future, sync::Arc};

use client_writer::trace_into_writer;
use futures::{FutureExt, TryFutureExt};
use thiserror::Error;
use tokio::{
    io::{split, AsyncReadExt, AsyncWriteExt, ReadHalf, WriteHalf},
    sync::Mutex,
};
use tracing::debug;

use crate::{
    nix_daemon::{
        ser::NixWriterBuilder,
        types::{NixError, StorePath},
    },
    worker_protocol::{server_handshake_client, Operation, Trust, STDERR_ERROR, STDERR_LAST},
};

use super::{
    de::NixReader,
    ser::{NixSerialize, NixWriter},
    worker_protocol::ClientSettings,
    NixDaemonIO, ProtocolVersion,
};
use crate::nix_daemon::de::NixRead;
use crate::nix_daemon::ser::NixWrite;

#[allow(dead_code)]
pub struct NixDaemon<R, W> {
    io: Arc<dyn NixDaemonIO + Sync + Send>,
    protocol_version: ProtocolVersion,
    client_settings: ClientSettings,
    reader: NixReader<R>,
    writer: Arc<Mutex<NixWriter<W>>>,
}

impl<R, W> NixDaemon<R, W> {
    pub fn new(
        io: Arc<dyn NixDaemonIO + Sync + Send>,
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

impl<RW> NixDaemon<ReadHalf<RW>, WriteHalf<RW>>
where
    RW: AsyncReadExt + AsyncWriteExt + Send + Unpin + 'static,
{
    /// Async constructor for NixDaemon.
    ///
    /// Performs the initial handshake with the client and retrieves the client's preferred
    /// settings.
    ///
    /// The resulting daemon can handle the client session by calling `handle_client()`.
    pub async fn initialize(
        io: Arc<dyn NixDaemonIO + Sync + Send>,
        mut connection: RW,
    ) -> Result<Self, std::io::Error>
    where
        RW: AsyncReadExt + AsyncWriteExt + Send + Unpin,
    {
        let protocol_version =
            server_handshake_client(&mut connection, "2.18.2", Trust::Trusted).await?;

        debug!("Client connected with protocol version {protocol_version}");
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
            let io = self.io.clone();
            if let Ok(operation) = TryInto::<Operation>::try_into(op_code) {
                match operation {
                    Operation::SetOptions => {
                        self.client_settings = self.reader.read_value().await?;
                        self.handle(async { Ok(()) }).await?
                    }
                    Operation::QueryPathInfo => {
                        let path: StorePath = self.reader.read_value().await?;
                        self.handle(io.query_path_info(&path).map_err(|r| r.into()))
                            .await?
                    }
                    Operation::IsValidPath => {
                        let path: StorePath = self.reader.read_value().await?;
                        self.handle(
                            io.query_path_info(&path)
                                .map(|p| p.map(|s| s.value.is_some()))
                                .map_err(|r| r.into()),
                        )
                        .await?
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
    async fn handle<T>(
        &mut self,
        future: impl Future<Output = Result<T, NixDaemonError>>,
    ) -> Result<(), std::io::Error>
    where
        T: NixSerialize + Send,
    {
        let result = trace_into_writer(
            self.client_settings.verbosity.into(),
            self.writer.clone(),
            future,
        )
        .await;
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

#[derive(Error, Debug)]
pub enum NixDaemonError {
    // Suggested code may be subject to a license. Learn more: ~LicenseLog:567863220.
    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),
    #[error("Operation `{0:?}` is not implemented yet.")]
    OperationNotImplemented(Operation),
    #[error("Unknown operation opcode `{0}`.")]
    UnknownOperation(u64),
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use async_trait::async_trait;
    use tokio::io::AsyncWriteExt;

    use crate::{
        nix_daemon::{
            containers::OptionWithPresence,
            ser::{NixWrite, NixWriter},
            server::NixDaemon,
            types, NixDaemonIO,
        },
        worker_protocol::{ClientSettings, WORKER_MAGIC_1, WORKER_MAGIC_2},
        ProtocolVersion,
    };

    struct MockDaemonIO {}

    #[async_trait]
    impl NixDaemonIO for MockDaemonIO {
        async fn query_path_info(
            &self,
            path: &types::StorePath,
        ) -> std::io::Result<OptionWithPresence<types::UnkeyedValidPathInfo>> {
            Ok(None.into())
        }
    }

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
