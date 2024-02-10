//! This module implements logic required for persisting known paths
//! during an evaluation.
//!
//! Tvix needs to be able to keep track of each Nix store path that it
//! knows about during the scope of a single evaluation and its
//! related builds.
//!
//! This data is required to find the derivation needed to actually trigger the
//! build, if necessary.

use nix_compat::{
    derivation::Derivation,
    nixhash::NixHash,
    store_path::{StorePath, StorePathRef},
};
use std::collections::{hash_map::Entry, HashMap};

/// Struct keeping track of all known Derivations in the current evaluation.
/// This keeps both the Derivation struct, as well as the "Hash derivation
/// modulo".
#[derive(Debug, Default)]
pub struct KnownPaths {
    /// All known derivation or FOD hashes.
    ///
    /// Keys are derivation paths, values are a tuple of the "hash derivation
    /// modulo" and the Derivation struct itself.
    derivations: HashMap<StorePath, (NixHash, Derivation)>,

    /// A map from output path to (one) drv path.
    /// Note that in the case of FODs, multiple drvs can produce the same output
    /// path. We use one of them
    outputs_to_drvpath: HashMap<StorePath, StorePath>,
}

impl KnownPaths {
    /// Fetch the opaque "hash derivation modulo" for a given derivation path.
    pub fn get_hash_derivation_modulo(&self, drv_path: &StorePathRef) -> Option<&NixHash> {
        self.derivations
            .get(&drv_path.to_owned())
            .map(|(hash_derivation_modulo, _derivation)| hash_derivation_modulo)
    }

    /// Return a reference to the Derivation for a given drv path.
    pub fn get_drv_by_drvpath(&self, drv_path: &StorePath) -> Option<&Derivation> {
        self.derivations
            .get(drv_path)
            .map(|(_hash_derivation_modulo, derivation)| derivation)
    }

    /// Return a &Derivation producing the passed output path.
    /// Note there can be multiple Derivations producing the same output path in
    /// flight; this function will only return one of them.
    pub fn get_drv_by_output_path(&self, output_path: &StorePath) -> Option<&Derivation> {
        let drv_path = self.outputs_to_drvpath.get(output_path)?;

        Some(&self.derivations[drv_path].1)
    }

    /// Insert a new Derivation into this struct.
    /// The Derivation struct must pass validation, and its output paths need to
    /// be fully calculated.
    /// All input derivations this refers to must also be inserted to this
    /// struct.
    pub fn add(&mut self, drv_path: StorePath, drv: Derivation) {
        // compute the hash derivation modulo
        let hash_derivation_modulo = drv.derivation_or_fod_hash(|drv_path| {
            self.get_hash_derivation_modulo(&drv_path)
                .expect(&format!("{} not found", drv_path))
                .to_owned()
        });

        // For all output paths, update our lookup table.
        // We only write into the lookup table once.
        for output in drv.outputs.values() {
            // We assume derivations to be passed validated, so ignoring rest
            // and expecting parsing is ok.
            // TODO: b/264
            let (output_path, _rest) =
                StorePath::from_absolute_path_full(&output.path).expect("parse output path");

            if let Entry::Vacant(entry) = self.outputs_to_drvpath.entry(output_path) {
                entry.insert(drv_path.to_owned());
            }
        }

        // insert the derivation itself
        #[allow(unused_variables)] // assertions on this only compiled in debug builds
        let old = self
            .derivations
            .insert(drv_path.to_owned(), (hash_derivation_modulo.clone(), drv));

        #[cfg(debug_assertions)]
        {
            if let Some(old) = old {
                debug_assert!(
                    old.0 == hash_derivation_modulo,
                    "hash derivation modulo for a given derivation should always be calculated the same"
                );
            }

            // TODO: check input derivations to have been inserted.
        }
    }
}
