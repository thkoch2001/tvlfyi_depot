pub mod worker_protocol;

mod protocol_version;
use async_trait::async_trait;
pub use protocol_version::ProtocolVersion;

pub mod containers;
pub mod de;
pub mod en;
pub mod server;
pub mod types;

#[async_trait]
pub trait NixDaemonIO {
    async fn query_path_info(
        &self,
        path: &types::StorePath,
    ) -> std::io::Result<Option<types::UnkeyedValidPathInfo>>;
}
