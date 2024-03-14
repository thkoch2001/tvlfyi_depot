//! Module parsing and emitting the wire format used by Nix, both in the
//! nix-daemon protocol as well as in the NAR format.

#[cfg(feature = "async")]
mod bytes;
#[cfg(feature = "async")]
mod primitive;
