use clap::Parser;
use tokio::io::{AsyncReadExt, AsyncWriteExt};
use tokio_listener::{self, SystemOptions, UserOptions};
use tracing::{debug, error, info, instrument, Level};

use nix_compat::wire::{
    primitive,
    worker_protocol::{self, server_handshake_client, ClientSettings, Trust},
};

#[derive(Parser, Debug)]
struct Cli {
    /// Listening unix socket path
    #[arg(short, long)]
    socket: Option<String>,
    /// Log verbosity level. Can be "error", "warn", "info", "debug", "trace", or a number 1-5
    #[arg(short, long, env)]
    verbosity: Option<Level>,
}

#[tokio::main]
#[instrument()]
async fn main() {
    let args = Cli::parse();
    tracing_subscriber::fmt()
        .compact()
        .with_max_level(
            args.verbosity
                .unwrap_or_else(|| panic!("Can't parse log verbosity")),
        )
        .try_init()
        .unwrap();
    info!("Started Tvix daemon");
    let addr = args
        .socket
        .unwrap_or_else(|| "sd_listen_unix".to_string())
        .parse()
        .expect("Invalid listening socket address");
    let system_options: SystemOptions = Default::default();
    let mut user_options: UserOptions = Default::default();
    user_options.recv_buffer_size = Some(1024);
    user_options.send_buffer_size = Some(1024);
    info!(user_options.send_buffer_size);
    info!(user_options.recv_buffer_size);
    let mut listener = tokio_listener::Listener::bind(&addr, &system_options, &user_options)
        .await
        .unwrap();
    info!(listener_address = ?listener, "Listening for incoming connections");
    while let Ok((conn, addr)) = listener.accept().await {
        info!(addr = %addr, "Incoming connection");
        tokio::spawn(async move { worker(conn).await });
    }
}

/// Structure used to hold the client socket connection and some
/// metadata about the connection.
#[derive(Debug)]
struct ClientConnection<R: AsyncReadExt + AsyncWriteExt + Unpin> {
    pub conn: R,
    pub version_minor: u64,
    pub client_settings: Option<ClientSettings>,
}

/// Worker in charge to respond a Nix client using the Nix wire
/// protocol.
#[instrument()]
async fn worker<R>(mut conn: R)
where
    R: AsyncReadExt + AsyncWriteExt + Unpin + std::fmt::Debug,
{
    match server_handshake_client(&mut conn, "2.18.2", Trust::Trusted).await {
        Ok(client_protocol_version) => {
            let mut client_connection = ClientConnection {
                conn,
                version_minor: client_protocol_version,
                client_settings: None,
            };
            debug!("Client hanshake succeeded");
            debug!(client_protocol_version = ?client_protocol_version);
            // TODO: implement logging. For now, we'll just send
            // STDERR_LAST, which is good enough to get Nix respond to
            // us.
            primitive::write_u64(&mut client_connection.conn, worker_protocol::STDERR_LAST)
                .await
                .unwrap();
            loop {
                let op = worker_protocol::read_op(&mut client_connection.conn)
                    .await
                    .unwrap();
                match op {
                    worker_protocol::Operation::SetOptions => {
                        let settings = op_set_options(&mut client_connection).await.unwrap();
                        client_connection.client_settings = Some(settings);
                        debug!(settings = ?client_connection.client_settings, "Received client settings");
                    }
                    _ => {
                        error!(op = ?op, "Unimplemented operation");
                        break;
                    }
                }
            }
        }
        Err(e) => error!("Client handshake failed: {}", e),
    }
}

async fn op_set_options<R>(conn: &mut ClientConnection<R>) -> std::io::Result<ClientSettings>
where
    R: AsyncReadExt + AsyncWriteExt + Unpin + std::fmt::Debug,
{
    let settings =
        worker_protocol::read_client_settings(&mut conn.conn, conn.version_minor).await?;
    // The client expects us to send some logs when we're processing
    // the settings. Sending STDERR_LAST signal we're done processing.
    primitive::write_u64(&mut conn.conn, worker_protocol::STDERR_LAST).await?;
    Ok(settings)
}
