pub mod worker_protocol;

mod protocol_version;
use std::io::Result;

pub use protocol_version::ProtocolVersion;
use types::UnkeyedValidPathInfo;

use crate::store_path::StorePath;

pub mod de;
pub mod handler;
pub mod ser;
pub mod types;

/// Represents all possible operations over the nix-daemon protocol.
pub trait NixDaemonIO {
    fn query_path_info(
        &self,
        path: &StorePath<String>,
    ) -> impl std::future::Future<Output = Result<Option<UnkeyedValidPathInfo>>> + Send;
}
