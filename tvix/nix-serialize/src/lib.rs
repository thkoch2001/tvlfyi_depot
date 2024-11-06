mod de;
mod protocol_version;
mod ser;

pub use de::{Error as DeserializeError, NixDeserialize, NixRead, NixReader, NixReaderBuilder};
pub use protocol_version::ProtocolVersion;
pub use ser::{Error as SerializeError, NixSerialize, NixWrite, NixWriter, NixWriterBuilder};

#[cfg(any(test, feature = "test"))]
pub use de::mock as de_mock;

#[cfg(any(test, feature = "test"))]
pub use ser::mock as ser_mock;

#[cfg(feature = "nix-serialize-derive")]
extern crate nix_serialize_derive;

#[cfg(feature = "nix-serialize-derive")]
#[cfg_attr(docsrs, doc(cfg(feature = "derive")))]
pub use nix_serialize_derive::{
    nix_deserialize_remote, nix_serialize_remote, NixDeserialize, NixSerialize,
};

/// 8 null bytes, used to write out padding.
pub(crate) const EMPTY_BYTES: &[u8; 8] = &[0u8; 8];
