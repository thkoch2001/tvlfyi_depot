use std::{error::Error, future::Future};

use ::bytes::Bytes;

use super::ProtocolVersion;

mod bytes;
mod collections;
mod int;

pub mod writer;

pub trait NixWrite: Send {
    type Error: Error + Send + Sync;

    fn version(&self) -> ProtocolVersion;

    fn write_number(&mut self, value: u64) -> impl Future<Output = Result<(), Self::Error>> + Send;

    fn write_bytes(&mut self, value: Bytes)
        -> impl Future<Output = Result<(), Self::Error>> + Send;

    fn write<V: NixSerialize + Send>(
        &mut self,
        value: V,
    ) -> impl Future<Output = Result<(), Self::Error>> + Send;
}

pub trait NixSerialize: Sized {
    fn serialize<W>(self, writer: &mut W) -> impl Future<Output = Result<(), W::Error>> + Send
    where
        W: ?Sized + NixWrite + Send;
}
