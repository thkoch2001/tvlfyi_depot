use derive_more::derive::Constructor;
use nix_compat_derive::{NixDeserialize, NixSerialize};

use crate::{
    narinfo::Signature,
    nix_daemon::{de::NixDeserialize, ser::NixSerialize},
    nixhash::CAHash,
    store_path::{StorePath, STORE_DIR_WITH_SLASH},
};

/// Marker type that consumes/sends and ignores a u64.
#[derive(Clone, Debug, NixDeserialize, NixSerialize)]
#[nix(from = "u64", into = "u64")]
pub struct IgnoredZero;
impl From<u64> for IgnoredZero {
    fn from(_: u64) -> Self {
        IgnoredZero
    }
}

impl From<IgnoredZero> for u64 {
    fn from(_: IgnoredZero) -> Self {
        0
    }
}

#[derive(Debug, NixSerialize)]
pub struct TraceLine {
    have_pos: IgnoredZero,
    hint: String,
}

/// Represents an error returned by the nix-daemon to its client.
///
/// Adheres to the format described in serialization.md
#[derive(NixSerialize)]
pub struct NixError {
    #[nix(version = "26..")]
    type_: &'static str,

    #[nix(version = "26..")]
    level: u64,

    #[nix(version = "26..")]
    name: &'static str,

    msg: String,
    #[nix(version = "26..")]
    have_pos: IgnoredZero,

    #[nix(version = "26..")]
    traces: Vec<TraceLine>,

    #[nix(version = "..=25")]
    exit_status: u64,
}

impl NixError {
    pub fn new(msg: String) -> Self {
        Self {
            type_: "Error",
            level: 0, // error
            name: "Error",
            msg,
            have_pos: IgnoredZero {},
            traces: vec![],
            exit_status: 1,
        }
    }
}

#[derive(NixSerialize, Debug, Constructor)]
pub struct UnkeyedValidPathInfo {
    pub deriver: Option<StorePath<String>>,
    pub nar_hash: String,
    pub references: Vec<StorePath<String>>,
    pub registration_time: u64,
    pub nar_size: u64,
    pub ultimate: bool,
    pub signatures: Vec<Signature<String>>,
    pub ca: Option<CAHash>,
}

nix_compat_derive::nix_serialize_remote!(#[nix(display)] Signature<String>);
nix_compat_derive::nix_serialize_remote!(
    #[nix(display)]
    CAHash
);

impl NixSerialize for Option<CAHash> {
    async fn serialize<W>(&self, writer: &mut W) -> Result<(), W::Error>
    where
        W: super::ser::NixWrite,
    {
        match self {
            Some(value) => writer.write_value(value).await,
            None => writer.write_value("").await,
        }
    }
}

impl NixSerialize for Option<UnkeyedValidPathInfo> {
    async fn serialize<W>(&self, writer: &mut W) -> Result<(), W::Error>
    where
        W: super::ser::NixWrite,
    {
        match self {
            Some(value) => {
                writer.write_value(&true).await?;
                writer.write_value(value).await
            }
            None => writer.write_value(&false).await,
        }
    }
}

// Custom implementation since FromStr does not use from_absolute_path
impl NixDeserialize for StorePath<String> {
    async fn try_deserialize<R>(reader: &mut R) -> Result<Option<Self>, R::Error>
    where
        R: ?Sized + crate::nix_daemon::de::NixRead + Send,
    {
        use crate::nix_daemon::de::Error;
        if let Some(buf) = reader.try_read_bytes().await? {
            let result = StorePath::<String>::from_absolute_path(&buf);
            result.map(Some).map_err(R::Error::invalid_data)
        } else {
            Ok(None)
        }
    }
}

// Custom implementation since Display does not use absolute paths.
impl NixSerialize for StorePath<String> {
    async fn serialize<W>(&self, writer: &mut W) -> Result<(), W::Error>
    where
        W: crate::nix_daemon::ser::NixWrite,
    {
        writer
            .write_value(&format!("{STORE_DIR_WITH_SLASH}{self}"))
            .await
    }
}

// Writes store-path or an empty string.
impl NixSerialize for Option<StorePath<String>> {
    async fn serialize<W>(&self, writer: &mut W) -> Result<(), W::Error>
    where
        W: crate::nix_daemon::ser::NixWrite,
    {
        match self {
            Some(value) => writer.write_value(value).await,
            None => writer.write_value("").await,
        }
    }
}

#[cfg(test)]
mod tests {}
