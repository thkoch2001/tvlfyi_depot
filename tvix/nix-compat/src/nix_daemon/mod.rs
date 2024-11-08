pub mod worker_protocol;

mod protocol_version;
pub use protocol_version::ProtocolVersion;

pub mod de;
pub mod handler;
pub mod ser;
pub mod types;

pub trait NixDaemonIO {
    // TODO add methods to it.
}
