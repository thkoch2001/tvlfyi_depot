use std::future::Future;

use super::ProtocolVersion;

mod bytes;
mod collections;
mod int;
pub mod writer;

/// A writer of data to the Nix daemon protocol.
/// Basically there are two basic types in the Nix daemon protocol
/// u64 and a bytes buffer. Everything else is more or less built on
/// top of these two types.
pub trait NixWrite: Send {
    type Error: std::error::Error + Send;

    /// Some types are serialized differently depending on the version
    /// of the protocol and so this can be used for implementing that.
    fn version(&self) -> ProtocolVersion;

    /// Write single u64 to the protocol.
    fn write_number(
        &mut self,
        value: u64,
    ) -> impl Future<Output = Result<(), Self::Error>> + Send + '_;

    /// Write bytes to the protocol.
    fn write_bytes<'a>(
        &'a mut self,
        value: &'a [u8],
    ) -> impl Future<Output = Result<(), Self::Error>> + Send + '_;

    /// Write a serializable value to the protocol.
    /// Uses `NixSerialize::serialize` to encode the value.
    fn write<'a, T: NixSerialize + Sync>(
        &'a mut self,
        value: &'a T,
    ) -> impl Future<Output = Result<(), Self::Error>> + Send + '_;
}

/// A data structure that can be serialized into the Nix daemon
/// worker protocol.
pub trait NixSerialize {
    fn serialize<'a, W>(
        &'a self,
        writer: &'a mut W,
    ) -> impl Future<Output = Result<(), W::Error>> + Send + '_
    where
        W: NixWrite + Send;
}
