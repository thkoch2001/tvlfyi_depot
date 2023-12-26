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
use std::{
    collections::{hash_map, BTreeSet, HashMap},
    ops::Index,
};
use tracing::warn;

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
    /// All known paths, keyed by a truncated version of their store
    /// path used for reference scanning.
    paths: HashMap<KnownPathKey, KnownPath>,
    multipaths: HashMap<PathName, Vec<KnownPath>>,

    /// All known derivation or FOD hashes.
    ///
    /// Keys are derivation paths, values is the NixHash.
    derivation_or_fod_hashes: HashMap<String, NixHash>,
}

impl Index<&PathName> for KnownPaths {
    type Output = Vec<KnownPath>;

    fn index(&self, index: &PathName) -> &Self::Output {
        &self.multipaths[index]
    }
}

impl KnownPaths {
    #[allow(dead_code)]
    fn strict_insert_path(&mut self, drv_name: String, path: String, path_kind: PathKind) {
        let truncated_path: PathName = path.as_str().into();
        match self
            .paths
            .entry((DrvName(drv_name.clone()), truncated_path))
        {
            hash_map::Entry::Vacant(entry) => {
                entry.insert(KnownPath::new(path, path_kind));
            }

            hash_map::Entry::Occupied(mut entry) => {
                match (path_kind, &mut entry.get_mut().kind) {
                    // These variant combinations require no "merging action".
                    (PathKind::Plain, PathKind::Plain) => (),

                    #[allow(unused_variables)]
                    (
                        PathKind::Output {
                            name: name1,
                            derivation: drv1,
                            ca_hash: ca_hash1,
                        },
                        PathKind::Output {
                            name: ref name2,
                            derivation: ref drv2,
                            ca_hash: ref ca_hash2,
                        },
                    ) => {
                        #[cfg(debug_assertions)]
                        {
                            if &name1 != name2 {
                                panic!(
                                    "inserted path {} with two different names: {} and {}",
                                    path, name1, name2
                                );
                            }
                            if &drv1 != drv2 && ca_hash1 != *ca_hash2 {
                                panic!(
                                    "inserted path {} with two different derivations: {} and {} under the single derivation name {} but with different CA hashes: {:?} and {:?}",
                                    path, drv1, drv2, drv_name, ca_hash1, ca_hash2
                                );
                            } else if &drv1 != drv2 {
                                println!(
                                    "inserted path {} with two different derivations but same CAhash ({:?}): {} and {} under the single derivation name {}",
                                    path, ca_hash1, drv1, drv2, drv_name);
                                println!("{}", std::backtrace::Backtrace::force_capture());
                            }
                        }
                    }

                    (
                        PathKind::Derivation { output_names: new },
                        PathKind::Derivation {
                            output_names: ref mut old,
                        },
                    ) => {
                        old.extend(new);
                    }

                    _ => panic!(
                        "path '{}' ({}) inserted twice with different types",
                        entry.key().1,
                        entry.key().0
                    ),
                };
            }
        };
    }
    fn insert_path(&mut self, drv_name: String, path: String, path_kind: PathKind) {
        let truncated_path: PathName = path.as_str().into();
        self.multipaths
            .entry(truncated_path.clone())
            .and_modify(|paths| {
                paths.push(KnownPath::new(path.clone(), path_kind.clone()));
            })
            .or_insert_with(|| vec![KnownPath::new(path.clone(), path_kind.clone())]);

        self.strict_insert_path(drv_name, path, path_kind);
    }

    /// Mark a plain path as known.
    pub fn plain<N: ToString, S: ToString>(&mut self, name: N, path: S) {
        self.insert_path(name.to_string(), path.to_string(), PathKind::Plain);
    }

    /// Mark a derivation as known.
    pub fn drv<N: ToString, P: ToString, O: ToString>(&mut self, name: N, path: P, outputs: &[O]) {
        self.insert_path(
            name.to_string(),
            path.to_string(),
            PathKind::Derivation {
                output_names: outputs.iter().map(ToString::to_string).collect(),
            },
        );
    }

    /// Mark a derivation output path as known.
    pub fn output<DN: ToString, P: ToString, N: ToString, D: ToString>(
        &mut self,
        drv_name: DN,
        output_path: P,
        output_cahash: &Option<CAHash>,
        name: N,
        drv_path: D,
    ) {
        self.insert_path(
            drv_name.to_string(),
            output_path.to_string(),
            PathKind::Output {
                name: name.to_string(),
                derivation: drv_path.to_string(),
                ca_hash: output_cahash.clone(),
            },
        );
    }

    /// Checks whether there are any known paths. If not, a reference
    /// scanner can not be created.
    pub fn is_empty(&self) -> bool {
        self.paths.is_empty()
    }

    /// Create a reference scanner from the current set of known paths.
    pub fn reference_scanner(&self) -> ReferenceScanner<PathName> {
        let candidates = self.paths.keys().map(|(_, key)| key.clone()).collect();
        ReferenceScanner::new(candidates)
    }

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
