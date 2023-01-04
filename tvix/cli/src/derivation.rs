//! Implements `builtins.derivation`, the core of what makes Nix build packages.

use std::collections::btree_map;
use std::collections::BTreeSet;
use tvix_derivation::Derivation;
use tvix_eval::builtin_macros::builtins;
use tvix_eval::{CoercionKind, ErrorKind, Value, VM};

use crate::known_paths::{KnownPaths, PathType};

// Constants used for strangely named fields in derivation inputs.

const STRUCTURED_ATTRS: &'static str = "__structuredAttrs";
const IGNORE_NULLS: &'static str = "__ignoreNulls";

#[builtins]
mod derivation_builtins {
    use super::*;

    use std::collections::BTreeMap;
    use tvix_derivation::{Hash, Output};
    use tvix_eval::NixList;

    /// Helper function for populating the `drv.outputs` field from a
    /// manually specified set of outputs, instead of the default
    /// `outputs`.
    fn populate_outputs(
        vm: &mut VM,
        outputs: &mut BTreeMap<String, Output>,
        arg: NixList,
    ) -> Result<(), ErrorKind> {
        for output in arg {
            let output_name = output.force(vm)?.to_str()?;

            if output_name.as_str() == "drv" {
                todo!("error: invalid output name")
            }

            // TODO: further validate output name
            if let Some(_) = outputs.insert(output_name.to_string(), Default::default()) {
                todo!("error: duplicate output")
            }
        }

        Ok(())
    }

    /// Strictly construct a Nix derivation from the supplied arguments.
    ///
    /// This is considered an internal function, users usually want to
    /// use the higher-level `builtins.derivation` instead.
    #[builtin("derivationStrict")]
    fn builtin_derivation_strict(vm: &mut VM, input: Value) -> Result<Value, ErrorKind> {
        let known_paths: &mut KnownPaths = todo!("thread this thing through");

        let input = input.to_attrs()?;
        let _name = input.select_required("name")?.force(vm)?.to_str()?;

        // Check whether attributes should be passed as a JSON file.
        // TODO: the JSON serialisation has to happen here.
        if let Some(sa) = input.select(STRUCTURED_ATTRS) {
            if sa.force(vm)?.as_bool()? {
                return Err(ErrorKind::NotImplemented(STRUCTURED_ATTRS));
            }
        }

        // Check whether null attributes should be ignored or passed through.
        let ignore_nulls = match input.select(IGNORE_NULLS) {
            Some(b) => b.force(vm)?.as_bool()?,
            None => false,
        };

        let mut drv = Derivation::default();
        drv.outputs.insert("out".to_string(), Default::default());

        // Check for fixed-output derivation keys.
        match (input.select("outputHash"), input.select("outputHashAlgo")) {
            (Some(hash), Some(algo)) => match drv.outputs.get_mut("out") {
                None => todo!("fixed-output & multiple outputs is an error"),
                Some(out) => {
                    out.hash = Some(Hash {
                        algo: algo
                            .force(vm)?
                            .coerce_to_string(CoercionKind::Strong, vm)?
                            .to_string(),

                        digest: hash
                            .force(vm)?
                            .coerce_to_string(CoercionKind::Strong, vm)?
                            .to_string(),
                    });
                }
            },

            (Some(_), None) | (None, Some(_)) => todo!("error: missing FOD fields"),
            (None, None) => {}
        }
        // TODO: outputHashMode? :thonking:

        // TODO: this should be a reference to this thing from the outside
        // enum PathType {
        //     /// Imported path (e.g. through `src = ./.`), goes into a
        //     /// derivation's `inputSrcs` field.
        //     Source,
        //     /// Derivation (or derivation output)
        //     Derived,
        // }
        //
        // let refscan = crate::refscan::ReferenceScanner::new(&[]);

        let refscan = known_paths.reference_scanner();

        for (name, value) in input.into_iter_sorted() {
            if ignore_nulls && matches!(*value.force(vm)?, Value::Null) {
                continue;
            }

            let val_str = value
                .force(vm)?
                .coerce_to_string(CoercionKind::Strong, vm)?
                .to_string();

            match name.as_str() {
                IGNORE_NULLS => continue,

                // Command line arguments to the builder.
                "args" => {
                    let args = value.to_list()?;
                    for arg in args {
                        drv.arguments.push(arg.force(vm)?.to_str()?.to_string());
                    }
                    continue;
                }

                // Explicitly specified drv outputs (instead of default [ "out" ])
                "outputs" => {
                    let outputs = value.to_list()?;
                    populate_outputs(vm, &mut drv.outputs, outputs)?;
                }

                "builder" => {
                    drv.builder = val_str.clone();
                }

                "system" => {
                    drv.system = val_str.clone();
                }

                _ => {}
            }

            // Scan references in this attribute to detect any build-references.
            refscan.scan_str(&val_str);

            // Most of these are also added to the builder's environment in "raw" form.
            if let Some(_) = drv.environment.insert(name.to_string(), val_str) {
                todo!("error: duplicate env in drv yada yada")
            }
        }

        let build_references = refscan.finalise();
        for reference in build_references.into_iter() {
            match known_paths[&reference] {
                PathType::Plain => drv.input_sources.push(reference.to_string()),

                PathType::Output { name, derivation } => {
                    match drv.input_derivations.entry(derivation) {
                        btree_map::Entry::Vacant(mut entry) => {
                            entry.insert(BTreeSet::from([name]));
                        }

                        btree_map::Entry::Occupied(mut entry) => {
                            entry.get_mut().insert(name);
                        }
                    }
                }

                PathType::Derivation { output_names } => {
                    match drv.input_derivations.entry(reference.to_string()) {
                        btree_map::Entry::Vacant(mut entry) => {
                            entry.insert(output_names);
                        }

                        btree_map::Entry::Occupied(mut entry) => {
                            entry.get_mut().extend(output_names);
                        }
                    }
                }
            }
        }

        todo!()
    }

    /// Construct a Nix derivation from the supplied arguments.
    ///
    /// Calling this function immediately causes a build to start.
    #[builtin("derivation")]
    fn builtin_derivation(_vm: &mut VM, _args: Value) -> Result<Value, ErrorKind> {
        Err(ErrorKind::NotImplemented("builtins.derivation"))
    }
}

pub fn derivation_builtins() -> Vec<(&'static str, Value)> {
    derivation_builtins::builtins()
}
