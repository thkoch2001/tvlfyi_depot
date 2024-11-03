use std::sync::Arc;

use thiserror::Error;
use tokio::io::{split, AsyncReadExt, AsyncWriteExt, ReadHalf, WriteHalf};
use tracing::debug;

use crate::{
    nix_daemon::{
        containers::OptionWithPresence,
        types::{NixError, StorePath, UnkeyedValidPathInfo},
    },
    worker_protocol::{server_handshake_client, Trust, Verbosity, STDERR_ERROR, STDERR_LAST},
};

use super::{
    de::NixReader,
    en::writer::NixWriter,
    protocol_version::ProtocolVersion,
    worker_protocol::{ClientSettings, Operation},
    NixDaemonIO,
};
use crate::nix_daemon::de::NixRead;
use crate::nix_daemon::en::NixWrite;

#[allow(dead_code)]
pub struct NixDaemon<R, W> {
    io: Arc<dyn NixDaemonIO + Sync + Send>,
    protocol_version: ProtocolVersion,
    client_settings: ClientSettings,
    reader: NixReader<R>,
    writer: NixWriter<W>,
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
            writer,
        }
    }
}

#[derive(Error, Debug)]
pub enum NixDaemonError {
    // Suggested code may be subject to a license. Learn more: ~LicenseLog:567863220.
    #[error("IO error: {0}")]
    Io(std::io::Error),
    #[error("Operation `{0:?}` is not implemented yet.")]
    OperationNotImplemented(Operation),
    #[error("Unknown operation opcode `{0}`.")]
    UnknownOperation(u64),
}

impl From<std::io::Error> for NixDaemonError {
    fn from(e: std::io::Error) -> Self {
        Self::Io(e)
    }
}

impl<RW> NixDaemon<ReadHalf<RW>, WriteHalf<RW>>
where
    RW: AsyncReadExt + AsyncWriteExt + Send + Unpin,
{
    pub async fn initialize(
        io: Arc<dyn NixDaemonIO + Sync + Send>,
        mut connection: RW,
    ) -> Result<Self, std::io::Error>
    where
        RW: AsyncReadExt + AsyncWriteExt + Send + Unpin,
    {
        debug!("Client connected");
        let protocol_version =
            server_handshake_client(&mut connection, "2.18.2", Trust::Trusted).await?;
        debug!("Client connected version {protocol_version}");
        connection.write_u64_le(STDERR_LAST).await?;
        let (reader, writer) = split(connection);
        let mut reader = NixReader::builder()
            .set_version(protocol_version)
            .build(reader);
        let mut writer = NixWriter::new(writer, protocol_version);

        let operation: Operation = reader.read_value().await?;
        debug!("Operation: {operation:?}");
        if operation != Operation::SetOptions {
            return Err(std::io::Error::other(
                "Expected SetOptions operation, but got {operation}",
            ));
        }
        let client_settings: ClientSettings = reader.read_value().await?;
        writer.write_number(STDERR_LAST).await?;
        debug!("Client settings: {client_settings:?}");
        Ok(Self::new(
            io,
            protocol_version,
            client_settings,
            reader,
            writer,
        ))
    }

    pub async fn handle_client(&mut self) -> Result<(), std::io::Error> {
        loop {
            let result = self.do_handle().await;
            match result {
                Ok(_) => (),
                Err(NixDaemonError::Io(e)) => {
                    debug!("IO error: {e:?}");
                    // TODO: at this point nix client hangs forever when trying to read the error with protocol version 1.26+
                    // need to debug
                    self.writer.write_number(STDERR_ERROR).await?;
                    self.writer
                        .write(&NixError::new(format!("{e:?}")))
                        .await?
                }
                Err(e) => {
                    return Err(std::io::Error::other(format!("{e:?}")));
                }
            }
        }
    }

    async fn do_handle(&mut self) -> Result<(), NixDaemonError> {
        debug!("Reading operation");
        let op_code = self.reader.read_number().await?;
        debug!("Read complete, operation: {op_code:?}");
        if let Ok(operation) = TryInto::<Operation>::try_into(op_code) {
            debug!("Operation: {operation:?}");
            match operation {
                Operation::SetOptions => {
                    self.client_settings = self.reader.read_value().await?;
                    self.writer.write_number(STDERR_LAST).await?
                }
                Operation::QueryPathInfo => {
                    let path: StorePath = self.reader.read_value().await?;
                    let resp: OptionWithPresence<UnkeyedValidPathInfo> =
                        self.io.query_path_info(&path).await?.into();
                    self.writer.write_number(STDERR_LAST).await?;
                    self.writer.write(&resp).await?
                }
                _ => {
                    return Err(NixDaemonError::OperationNotImplemented(operation));
                }
            }
            return Ok(());
        }
        Err(NixDaemonError::UnknownOperation(op_code))
    }
}
