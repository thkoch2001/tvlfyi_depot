use anyhow::anyhow;
use tokio::io::{AsyncReadExt, AsyncWriteExt};
use tokio_listener::{self, ListenerAddress, SystemOptions, UserOptions};
use tracing::{error, info, instrument};

use nix_compat::wire::primitive;

#[tokio::main]
#[instrument()]
async fn main() {
    tracing_subscriber::fmt()
        .compact()
        .try_init()
        .unwrap();


    info!("Started Tvix daemon");
    let addr: ListenerAddress = "sd_listen_unix".parse().unwrap();
    let system_options : SystemOptions = Default::default();
    let user_options : UserOptions = Default::default();
    let mut listener = tokio_listener::Listener::bind(&addr, &system_options, &user_options).await.unwrap();
    info!("Listening for incoming connections on {:?}", listener);
    while let Ok((conn, addr)) =  listener.accept().await {
        info!("Incoming connection from {addr}");
        tokio::spawn(async move {
            worker(conn).await
        });
    }
}

/// Worker in charge to respond a Nix client using the Nix wire
/// protocol.
#[instrument()]
async fn worker<R>(mut conn: R)
where
    R: AsyncReadExt + AsyncWriteExt + Unpin + std::fmt::Debug
{
    match perform_init_handshake(&mut conn).await {
        Ok(_) => {
            // TODO: process request here, dispatch to operation
            // handler.
            info!("Handshake done, bye now");
        },
        Err(e) => error!("Client handshake failed: {}", e),
    }
}

/// Performs the initial handshake. During the handshake, the client
/// will first send a magic u64, to which the daemon needs to respond
/// with another magic u64.
async fn perform_init_handshake<'a, R: 'a>(mut conn: &'a mut R) -> anyhow::Result<()>
where
    &'a mut R: AsyncReadExt + AsyncWriteExt + Unpin
{
    let magic_hello = conn.read_u64().await?;
    if magic_hello != primitive::MAGIC_HELLO {
        Err(anyhow!("Invalid server hello received: {}", magic_hello))
    } else {
        conn.write_u64(primitive::MAGIC_HELLO_RESPONSE).await?;
        Ok(())
    }
}

#[cfg(test)]
mod integration_tests {
    use nix_compat::wire::primitive;
    #[tokio::test]
    async fn test_init_handshake() {
        let mut test_conn = tokio_test::io::Builder::new()
            .read(&primitive::MAGIC_HELLO.to_be_bytes())
            .write(&primitive::MAGIC_HELLO_RESPONSE.to_be_bytes())
            .build();
        crate::worker(&mut test_conn).await;
    }
}
