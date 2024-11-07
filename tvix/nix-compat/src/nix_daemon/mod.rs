pub mod worker_protocol;

mod protocol_version;
pub use protocol_version::ProtocolVersion;

mod de;
mod ser;

pub use de::{Error as DeserializationError, NixDeserialize, NixRead, NixReader, NixReaderBuilder};
pub use ser::{Error as SerializationError, NixSerialize, NixWrite, NixWriter, NixWriterBuilder};

#[cfg(any(test, feature = "test"))]
pub use de::mock as de_mock;

#[cfg(any(test, feature = "test"))]
pub use ser::mock as ser_mock;

#[cfg(feature = "nix-compat-derive")]
extern crate nix_compat_derive;

#[cfg(feature = "nix-compat-derive")]
#[cfg_attr(docsrs, doc(cfg(feature = "wire")))]
pub use nix_compat_derive::{
    nix_deserialize_remote, nix_serialize_remote, NixDeserialize, NixSerialize,
};
