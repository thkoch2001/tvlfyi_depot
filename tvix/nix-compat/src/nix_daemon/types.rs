use derive_more::derive::Constructor;
use nix_compat_derive::{NixDeserialize, NixSerialize};

use super::{containers::OptionOrDefault, worker_protocol::Verbosity};

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
    level: Verbosity,

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
    deriver: OptionOrDefault<StorePath>,
    nar_hash: String,
    references: Vec<StorePath>,
    registration_time: u64,
    nar_size: u64,
    ultimate: bool,
    signatures: Vec<String>,
    ca: OptionOrDefault<String>,
}

impl NixError {
    pub fn new(level: Verbosity, msg: String) -> Self {
        Self {
            type_: "Error",
            level,
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
