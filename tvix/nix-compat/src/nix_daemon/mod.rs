pub mod worker_protocol;

mod protocol_version;
pub use protocol_version::ProtocolVersion;

pub mod de;
pub mod handler;
pub mod ser;
pub mod types;

/// Represents all possible operations over the nix-daemon protocol.
pub trait NixDaemonIO {
    // TODO add methods to it.
}
