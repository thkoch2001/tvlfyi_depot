pub mod worker_protocol;

mod protocol_version;
use async_trait::async_trait;
use containers::OptionWithPresence;
pub use protocol_version::ProtocolVersion;

pub mod containers;
pub mod de;
pub mod ser;
pub mod server;
pub mod types;

/// This trait represents all operations supported by the nix-daemon protocol.
#[async_trait]
pub trait NixDaemonIO {
    async fn query_path_info(
        &self,
        path: &types::StorePath,
    ) -> std::io::Result<OptionWithPresence<types::UnkeyedValidPathInfo>>;
}
