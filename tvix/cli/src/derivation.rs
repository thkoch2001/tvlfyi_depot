//! Implements `builtins.derivation`, the core of what makes Nix build packages.

use std::cell::RefCell;
use std::collections::btree_map;
use std::collections::BTreeSet;
use std::rc::Rc;
use tvix_derivation::Derivation;
use tvix_eval::builtin_macros::builtins;
use tvix_eval::{AddContext, CoercionKind, ErrorKind, NixAttrs, Value, VM};

use crate::errors::Error;
use crate::known_paths::{KnownPaths, PathType};

// Constants used for strangely named fields in derivation inputs.

const STRUCTURED_ATTRS: &'static str = "__structuredAttrs";
const IGNORE_NULLS: &'static str = "__ignoreNulls";

#[builtins(state = "Rc<RefCell<KnownPaths>>")]
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
            let output_name = output
                .force(vm)?
                .to_str()
                .context("determining output name")?;

            if let Some(_) = outputs.insert(output_name.as_str().into(), Default::default()) {
                return Err(Error::DuplicateOutput(output_name.as_str().into()).into());
            }
        }

        Ok(())
    }

    /// Populate the inputs of a derivation from the build references
    /// found when scanning the derivation's parameters.
    fn populate_inputs<I: IntoIterator<Item = String>>(
        drv: &mut Derivation,
        known_paths: &KnownPaths,
        references: I,
    ) {
        for reference in references.into_iter() {
            match &known_paths[&reference] {
                PathType::Plain => {
                    drv.input_sources.insert(reference.to_string());
                }

                PathType::Output { name, derivation } => {
                    match drv.input_derivations.entry(derivation.clone()) {
                        btree_map::Entry::Vacant(entry) => {
                            entry.insert(BTreeSet::from([name.clone()]));
                        }

                        btree_map::Entry::Occupied(mut entry) => {
                            entry.get_mut().insert(name.clone());
                        }
                    }
                }

                PathType::Derivation { output_names } => {
                    match drv.input_derivations.entry(reference.to_string()) {
                        btree_map::Entry::Vacant(entry) => {
                            entry.insert(output_names.clone());
                        }

                        btree_map::Entry::Occupied(mut entry) => {
                            entry.get_mut().extend(output_names.clone().into_iter());
                        }
                    }
                }
            }
        }
    }

    /// Populate the output configuration of a derivation based on the
    /// parameters passed to the call, flipping the required
    /// parameters for a fixed-output derivation if necessary.
    ///
    /// This function handles all possible combinations of the
    /// parameters, including invalid ones.
    fn populate_output_configuration(
        drv: &mut Derivation,
        vm: &mut VM,
        hash: Option<&Value>,      // in nix: outputHash
        hash_algo: Option<&Value>, // in nix: outputHashAlgo
        hash_mode: Option<&Value>, // in nix: outputHashmode
    ) -> Result<(), ErrorKind> {
        match (hash, hash_algo, hash_mode) {
            (Some(hash), Some(algo), hash_mode) => match drv.outputs.get_mut("out") {
                None => return Err(Error::ConflictingOutputTypes.into()),
                Some(out) => {
                    let algo = algo
                        .force(vm)?
                        .coerce_to_string(CoercionKind::Strong, vm)?
                        .to_string();

                    let hash_mode = match hash_mode {
                        None => None,
                        Some(mode) => Some(
                            mode.force(vm)?
                                .coerce_to_string(CoercionKind::Strong, vm)?
                                .as_str()
                                .to_string(),
                        ),
                    };

                    let algo = match hash_mode.as_deref() {
                        None | Some("flat") => algo,
                        Some("recursive") => format!("r:{algo}"),
                        Some(other) => {
                            return Err(Error::InvalidOutputHashMode(other.to_string()).into())
                        }
                    };

                    out.hash = Some(Hash {
                        algo,

                        digest: hash
                            .force(vm)?
                            .coerce_to_string(CoercionKind::Strong, vm)?
                            .to_string(),
                    });
                }
            },

            _ => {}
        }

        Ok(())
    }

    /// Handles derivation parameters which are not just forwarded to
    /// the environment. The return value indicates whether the
    /// parameter should be included in the environment.
    fn handle_derivation_parameters(
        drv: &mut Derivation,
        vm: &mut VM,
        name: &str,
        value: &Value,
        val_str: &str,
    ) -> Result<bool, ErrorKind> {
        match name {
            IGNORE_NULLS => return Ok(false),

            // Command line arguments to the builder.
            "args" => {
                let args = value.to_list()?;
                for arg in args {
                    drv.arguments.push(
                        arg.force(vm)?
                            .coerce_to_string(CoercionKind::Strong, vm)
                            .context("handling command-line builder arguments")?
                            .to_string(),
                    );
                }

                // The arguments do not appear in the environment.
                return Ok(false);
            }

            // Explicitly specified drv outputs (instead of default [ "out" ])
            "outputs" => {
                let outputs = value
                    .to_list()
                    .context("looking at the `outputs` parameter of the derivation")?;

                drv.outputs.clear();
                populate_outputs(vm, &mut drv.outputs, outputs)?;
            }

            "builder" => {
                drv.builder = val_str.to_string();
            }

            "system" => {
                drv.system = val_str.to_string();
            }

            _ => {}
        }

        Ok(true)
    }

    /// Strictly construct a Nix derivation from the supplied arguments.
    ///
    /// This is considered an internal function, users usually want to
    /// use the higher-level `builtins.derivation` instead.
    #[builtin("derivationStrict")]
    fn builtin_derivation_strict(
        state: Rc<RefCell<KnownPaths>>,
        vm: &mut VM,
        input: Value,
    ) -> Result<Value, ErrorKind> {
        let input = input.to_attrs()?;
        let name = input
            .select_required("name")?
            .force(vm)?
            .to_str()
            .context("determining derivation name")?;

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

        // Configure fixed-output derivations if required.
        populate_output_configuration(
            &mut drv,
            vm,
            input.select("outputHash"),
            input.select("outputHashAlgo"),
            input.select("outputHashMode"),
        )?;

        for (name, value) in input.clone().into_iter_sorted() {
            if ignore_nulls && matches!(*value.force(vm)?, Value::Null) {
                continue;
            }

            let val_str = value
                .force(vm)?
                .coerce_to_string(CoercionKind::Strong, vm)?
                .as_str()
                .to_string();

            handle_derivation_parameters(&mut drv, vm, name.as_str(), &value, &val_str)?;

            // Most of these are also added to the builder's environment in "raw" form.
            if let Some(_) = drv.environment.insert(name.as_str().to_string(), val_str) {
                return Err(Error::DuplicateEnvVar(name.as_str().to_string()).into());
            }
        }

        // Scan references in relevant attributes to detect any build-references.
        let mut refscan = state.borrow().reference_scanner();
        drv.arguments.iter().for_each(|s| refscan.scan_str(s));
        drv.environment.values().for_each(|s| refscan.scan_str(s));
        refscan.scan_str(&drv.builder);

        // Each output name needs to exist in the environment, at this
        // point initialised as an empty string because that is the
        // way of Golang ;)
        for output in drv.outputs.keys() {
            if let Some(_) = drv.environment.insert(output.to_string(), String::new()) {
                return Err(Error::ShadowedOutput(output.to_string()).into());
            }
        }

        let mut known_paths = state.borrow_mut();
        populate_inputs(&mut drv, &known_paths, refscan.finalise());

        // At this point, derivation fields are fully populated from
        // eval data structures.
        drv.validate(false).map_err(Error::InvalidDerivation)?;

        let tmp_replacement_str =
            drv.calculate_drv_replacement_str(|drv| known_paths.get_replacement_string(drv));

        drv.calculate_output_paths(&name, &tmp_replacement_str)
            .map_err(Error::InvalidDerivation)?;

        let actual_replacement_str =
            drv.calculate_drv_replacement_str(|drv| known_paths.get_replacement_string(drv));

        let derivation_path = drv
            .calculate_derivation_path(&name)
            .map_err(Error::InvalidDerivation)?;

        known_paths
            .add_replacement_string(derivation_path.to_absolute_path(), &actual_replacement_str);

        // mark all the new paths as known
        let output_names: Vec<String> = drv.outputs.keys().map(Clone::clone).collect();
        known_paths.drv(derivation_path.to_absolute_path(), &output_names);

        for (output_name, output) in &drv.outputs {
            known_paths.output(
                &output.path,
                output_name,
                derivation_path.to_absolute_path(),
            );
        }

        let mut new_attrs: Vec<(String, String)> = drv
            .outputs
            .into_iter()
            .map(|(name, output)| (name, output.path))
            .collect();

        new_attrs.push(("drvPath".to_string(), derivation_path.to_absolute_path()));

        Ok(Value::Attrs(Box::new(NixAttrs::from_iter(
            new_attrs.into_iter(),
        ))))
    }
}

pub fn derivation_builtins(known_paths: Rc<RefCell<KnownPaths>>) -> Vec<(&'static str, Value)> {
    derivation_builtins::builtins(known_paths)
}

pub fn derivation_src_builtins() -> Vec<(&'static str, &'static str)> {
    vec![(
        "derivation",
        r#"
# LGPL-2.1-or-later

/* This is the implementation of the ‘derivation’ builtin function.
   It's actually a wrapper around the ‘derivationStrict’ primop. */

drvAttrs @ { outputs ? [ "out" ], ... }:

let

  strict = derivationStrict drvAttrs;

  commonAttrs = drvAttrs // (builtins.listToAttrs outputsList) //
    { all = map (x: x.value) outputsList;
      inherit drvAttrs;
    };

  outputToAttrListElement = outputName:
    { name = outputName;
      value = commonAttrs // {
        outPath = builtins.getAttr outputName strict;
        drvPath = strict.drvPath;
        type = "derivation";
        inherit outputName;
      };
    };

  outputsList = map outputToAttrListElement outputs;

in (builtins.head outputsList).value
"#,
    )]
}
