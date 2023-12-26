//! This module implements logic required for persisting known paths
//! during an evaluation.
//!
//! Tvix needs to be able to keep track of each Nix store path that it
//! knows about during the scope of a single evaluation and its
//! related builds.
//!
//! This data is required to scan derivation inputs for the build
//! references (the "build closure") that they make use of.
//!
//! Please see //tvix/eval/docs/build-references.md for more
//! information.

use crate::refscan::{ReferenceScanner, STORE_PATH_LEN};
use nix_compat::nixhash::{CAHash, NixHash};
use std::collections::{BTreeSet, HashMap};

#[derive(Debug, PartialEq, Clone)]
pub enum PathKind {
    /// A literal derivation (`.drv`-file), and the *names* of its outputs.
    Derivation { output_names: BTreeSet<String> },

    /// An output of a derivation, its name, the path of its derivation
    /// and its CA hash if available.
    Output {
        name: String,
        derivation: String,
        ca_hash: Option<CAHash>,
    },

    /// A plain store path (e.g. source files copied to the store).
    Plain,
}

#[derive(Debug, PartialEq)]
pub struct KnownPath {
    pub path: String,
    pub kind: PathKind,
}

impl KnownPath {
    fn new(path: String, kind: PathKind) -> Self {
        KnownPath { path, kind }
    }
}

/// Internal struct to prevent accidental leaks of the truncated path
/// names.
#[repr(transparent)]
#[derive(Clone, Debug, Default, PartialEq, PartialOrd, Ord, Eq, Hash)]
pub struct PathName(String);

/// Internal struct to prevent accidental misuse of a random string
/// as a derivation name.
#[repr(transparent)]
#[derive(Clone, Debug, Default, PartialEq, PartialOrd, Ord, Eq, Hash)]
pub struct DrvName(String);

impl std::fmt::Display for PathName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl std::fmt::Display for DrvName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// It is possible to have multiple FODs referring to different derivations.
/// For example, a `src` attribute could use the same FOD but a different `version` string
/// and therefore a different `name` string.
/// To avoid conflating those two known paths, we key over the 2-uple (derivation name, truncated
/// path name).
/// See cl/9364 for more information.
type KnownPathKey = (DrvName, PathName);

impl From<&str> for PathName {
    fn from(s: &str) -> Self {
        PathName(s[..STORE_PATH_LEN].to_string())
    }
}

impl From<String> for PathName {
    fn from(s: String) -> Self {
        s.as_str().into()
    }
}

/// This instance is required to pass PathName instances as needles to
/// the reference scanner.
impl AsRef<[u8]> for PathName {
    fn as_ref(&self) -> &[u8] {
        self.0.as_ref()
    }
}

#[derive(Debug, Default)]
pub struct KnownPaths {
    /// All known derivation or FOD hashes.
    ///
    /// Keys are derivation paths, values is the NixHash.
    derivation_or_fod_hashes: HashMap<String, NixHash>,
}

impl KnownPaths {
    /// Fetch the opaque "hash derivation modulo" for a given derivation path.
    pub fn get_hash_derivation_modulo(&self, drv_path: &str) -> NixHash {
        // TODO: we rely on an invariant that things *should* have
        // been calculated if we get this far.
        self.derivation_or_fod_hashes[drv_path].clone()
    }

    pub fn add_hash_derivation_modulo<D: ToString>(
        &mut self,
        drv: D,
        hash_derivation_modulo: &NixHash,
    ) {
        #[allow(unused_variables)] // assertions on this only compiled in debug builds
        let old = self
            .derivation_or_fod_hashes
            .insert(drv.to_string(), hash_derivation_modulo.to_owned());

        #[cfg(debug_assertions)]
        {
            if let Some(old) = old {
                debug_assert!(
                    old == *hash_derivation_modulo,
                    "hash derivation modulo for a given derivation should always be calculated the same"
                );
            }
        }
    }
}
