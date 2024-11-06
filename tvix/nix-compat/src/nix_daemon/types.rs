use derive_more::derive::Constructor;
use nix_compat_derive::{NixDeserialize, NixSerialize};

use super::containers::OptionOrDefault;

#[derive(Debug, NixSerialize)]
pub struct TraceLine {
    have_pos: u64,
    hint: String,
}

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
    have_pos: u64,

    #[nix(version = "26..")]
    traces: Vec<TraceLine>,

    #[nix(version = "..=25")]
    exit_status: u64,
}

#[derive(NixSerialize, Debug, Constructor)]
pub struct UnkeyedValidPathInfo {
    pub deriver: OptionOrDefault<StorePath>,
    pub nar_hash: String,
    pub references: Vec<StorePath>,
    pub registration_time: u64,
    pub nar_size: u64,
    pub ultimate: bool,
    pub signatures: Vec<String>,
    pub ca: OptionOrDefault<String>,
}

impl NixError {
    pub fn new(msg: String) -> Self {
        Self {
            type_: "Error",
            level: 0, // LvlError
            name: "Error",
            msg,
            have_pos: 0,
            traces: vec![],
            exit_status: 1,
        }
    }
}

#[derive(NixDeserialize, NixSerialize, Debug, Default, Constructor)]
pub struct StorePath {
    pub path: String,
}

#[derive(NixDeserialize, NixSerialize, Debug)]
pub struct QueryPathInfo {
    path: StorePath,
}

#[derive(NixDeserialize)]
pub struct QueryValidPaths {
    pub paths: Vec<StorePath>,
    #[nix(version = "27..")]
    pub substitute: bool,
}
