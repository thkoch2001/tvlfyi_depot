//! Parser for the Nix archive listing format, aka .ls.
//!
//! LS files are produced by the C++ Nix implementation via `write-nar-listing=1` query parameter
//! passed to a store implementation when transferring store paths.
//!
//! Listing files contains metadata about a file and its offset in the corresponding NAR.
//!
//! LS files do not offer any integrity field, validating the contents is the caller's
//! responsibility.

use std::collections::HashMap;

use serde::Deserialize;

#[cfg(test)]
mod test;

#[derive(Debug, Deserialize)]
#[serde(tag = "type", rename_all = "lowercase")]
pub enum ListingEntry {
    Regular {
        size: u64,
        #[serde(default)]
        executable: bool,
        #[serde(rename = "narOffset")]
        nar_offset: u64,
    },
    Directory {
        entries: HashMap<String, ListingEntry>,
    },
    Symlink {
        target: String,
    },
}

#[derive(Debug, Deserialize)]
pub struct Listing {
    pub root: ListingEntry,
    pub version: u64,
}
