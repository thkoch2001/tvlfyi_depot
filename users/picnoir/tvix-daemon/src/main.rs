use anyhow::anyhow;
use clap::Parser;
use tokio::io::{AsyncReadExt, AsyncWriteExt};
use tokio_listener::{self, SystemOptions, UserOptions};
use tracing::{debug, error, info, instrument};

use nix_compat::wire::{bytes, primitive, worker_protocol};

#[derive(Parser, Debug)]
struct Cli {
    /// Listening unix socket path
    #[arg(short, long)]
    socket: Option<String>,
}

#[tokio::main]
#[instrument()]
async fn main() {
    tracing_subscriber::fmt().compact().try_init().unwrap();
    let args = Cli::parse();

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

/// Worker in charge to respond a Nix client using the Nix wire
/// protocol.
#[instrument()]
async fn worker<R>(mut conn: R)
where
    R: AsyncReadExt + AsyncWriteExt + Unpin + std::fmt::Debug,
{
    match perform_init_handshake(&mut conn).await {
        Ok(_) => {
            info!("Client hanshake succeeded");
            // TODO: implement logging. For now, we'll just send
            // STDERR_LAST, which is good enough to get Nix respond to
            // us.
            primitive::write_u32(&mut conn, worker_protocol::STDERR_LAST)
                .await
                .unwrap();
            //
            let op = worker_protocol::read_op(&mut conn).await.unwrap();
            info!(op = ?op, "Operation received");
        }
        Err(e) => error!("Client handshake failed: {}", e),
    }
}

/// Performs the initial handshake. During the handshake, the client
/// will first send a magic u64, to which the daemon needs to respond
/// with another magic u64.
///
/// We then retrieve the client version, and discard a bunch of now
/// obsolete data.
#[instrument()]
async fn perform_init_handshake<'a, R: 'a>(mut conn: &'a mut R) -> anyhow::Result<()>
where
    &'a mut R: AsyncReadExt + AsyncWriteExt + Unpin + std::fmt::Debug,
{
    let mut magic_hello = vec![0; 8];
    conn.read(&mut magic_hello).await?;
    debug!("Hello read");
    if magic_hello != primitive::MAGIC_HELLO {
        Err(anyhow!(
            "Invalid client hello received: {:?}, expected {:?}",
            magic_hello,
            primitive::MAGIC_HELLO
        ))
    } else {
        conn.write(&primitive::MAGIC_HELLO_RESPONSE).await?;
        conn.write(&primitive::PROTOCOL_VERSION).await?;
        conn.flush().await?;
        debug!("Hello responded");
        let client_version = primitive::read_u32(&mut conn).await?;
        debug!("Version read");
        if client_version < 0x10a {
            return Err(anyhow!("The nix client version is too old"));
        }
        let protocol_minor = client_version & 0x00ff;
        let protocol_major = client_version & 0xff00;
        debug!(client.version = %client_version, client.minor = %protocol_minor, client.major = %protocol_major);
        // TODO: find a more efficient way to discard data. We're
        // pointlessly allocating data here.
        if protocol_minor >= 14 {
            debug!("read cpu affinity");
            // Obsolete CPU affinity.
            let read_affinity = primitive::read_u32(&mut conn).await?;
            if read_affinity != 0 {
                skip_8_bytes(&mut conn).await?;
            };
        }
        if protocol_minor >= 11 {
            // Obsolete reserveSpace
            debug!("read reservespace");
            skip_8_bytes(&mut conn).await?;
        }
        if protocol_minor >= 33 {
            // Nix version. We're plain lying, we're not Nix, but eh…
            // Setting it to the 2.3 lineage. Not 100% sure this is a
            // good idea.
            debug!("write version");
            // Plain str padded to 64 bits.
            bytes::write_bytes(&mut conn, "2.3.17".as_bytes()).await?;
            conn.flush().await?;
        }
        worker_protocol::write_worker_trust_level(&mut conn, worker_protocol::Trust::Trusted)
            .await?;
        info!("Trust sent");
        Ok(())
    }
}

async fn skip_8_bytes<R>(conn: &mut R) -> anyhow::Result<()>
where
    R: AsyncReadExt + Unpin + std::fmt::Debug
{
    let mut _discard_buffer = [0; 8];
    conn.read_exact(&mut _discard_buffer).await?;
    Ok(())
}

#[cfg(test)]
mod integration_tests {
    use nix_compat::wire::primitive;
    #[tokio::test]
    async fn test_init_handshake() {
        let mut test_conn = tokio_test::io::Builder::new()
            .read(&primitive::MAGIC_HELLO)
            .write(&primitive::MAGIC_HELLO_RESPONSE)
            .write(&primitive::PROTOCOL_VERSION)
            // Let's say the client is in sync with the daemon
            // protocol-wise
            .read(&primitive::PROTOCOL_VERSION)
            // cpu affinity
            .read(&vec![0; 8])
            // reservespace
            .read(&vec![0; 8])
            // version (size)
            .write(&vec![0x06, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00])
            // version (data == 2.2.17 + padding)
            .write(&vec![50, 46, 51, 46, 49, 55, 0, 0])
            // Trusted (1 == client trusted
            .write(&vec![1, 0, 0, 0, 0, 0, 0, 0])
            .build();
        crate::perform_init_handshake(&mut test_conn).await.unwrap();
    }
}
