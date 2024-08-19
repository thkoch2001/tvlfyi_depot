//! Parser for the Nix archive listing format, aka .ls.
//!
//! LS files are produced by the C++ Nix implementation via `write-nar-listing=1` query parameter
//! passed to a store implementation when transferring store paths.
//!
//! Listing files contains metadata about a file and its offset in the corresponding NAR.
//!
//! NOTE: LS entries does not offer any integrity field to validate the retrieved file at the provided
//! offset. Validating the contents is the caller's responsibility.

use std::{
    collections::HashMap,
    path::{Component, Path},
};

use serde::Deserialize;

#[cfg(test)]
mod test;

#[derive(Debug, thiserror::Error)]
pub enum ListingError {
    // TODO: add an enum of what component was problematic
    // reusing `std::path::Component` is not possible as it contains a lifetime.
    #[error("unsupported path component (e.g. `..`, `/`, `C:\\`) encountered")]
    UnsupportedPathComponent,
}

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

impl ListingEntry {
    pub fn locate<P: AsRef<Path>>(&self, path: P) -> Result<Option<&ListingEntry>, ListingError> {
        // We perform a simple DFS on the components of the path
        // while rejecting dangerous components, e.g. `..` or `/`
        // Files and symlinks are *leaves*, i.e. we return them
        let mut cur = self;
        for component in path.as_ref().components() {
            println!("{:?}", component);
            match component {
                Component::CurDir => continue,
                Component::RootDir => return Err(ListingError::UnsupportedPathComponent),
                Component::Prefix(_) => return Err(ListingError::UnsupportedPathComponent),
                Component::ParentDir => return Err(ListingError::UnsupportedPathComponent),
                Component::Normal(file_or_dir_name) => {
                    if let Self::Directory { entries } = cur {
                        if let Some(new_entry) =
                            entries.get(file_or_dir_name.to_string_lossy().as_ref())
                        {
                            cur = new_entry;
                        } else {
                            return Ok(None);
                        }
                    } else {
                        return Ok(None);
                    }
                }
            }
        }

        // By construction, we found the node that corresponds to the path traversal.
        Ok(Some(cur))
    }
}

#[derive(Debug)]
pub struct ListingVersion<const V: u8>;

#[derive(Debug, thiserror::Error)]
#[error("Invalid version: {0}")]
struct ListingVersionError(u8);

impl<'de, const V: u8> Deserialize<'de> for ListingVersion<V> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let value = u8::deserialize(deserializer)?;
        if value == V {
            Ok(ListingVersion::<V>)
        } else {
            Err(serde::de::Error::custom(ListingVersionError(value)))
        }
    }
}

#[derive(Debug, Deserialize)]
#[serde(untagged)]
#[non_exhaustive]
pub enum Listing {
    V1 {
        root: ListingEntry,
        version: ListingVersion<1>,
    },
}
