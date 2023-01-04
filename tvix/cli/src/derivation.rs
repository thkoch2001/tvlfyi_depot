//! Implements `builtins.derivation`, the core of what makes Nix build packages.

use tvix_eval::builtin_macros::builtins;
use tvix_eval::{CoercionKind, ErrorKind, Value, VM};

// Constants used for strangely named fields in derivation inputs.

const STRUCTURED_ATTRS: &'static str = "__structuredAttrs";
const IGNORE_NULLS: &'static str = "__ignoreNulls";

#[builtins]
mod derivation_builtins {
    use super::*;

    /// Strictly construct a Nix derivation from the supplied arguments.
    ///
    /// This is considered an internal function, users usually want to
    /// use the higher-level `builtins.derivation` instead.
    #[builtin("derivationStrict")]
    fn builtin_derivation_strict(vm: &mut VM, input: Value) -> Result<Value, ErrorKind> {
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

        let mut drv = tvix_derivation::Derivation::default();
        drv.outputs.insert("out".to_string(), Default::default());

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
                    todo!("replace outputs")
                }

                "builder" => {
                    drv.builder = val_str.clone();
                }

                "system" => {
                    // drv.platform = val_str.clone();
                }

                "outputHash" => {
                    todo!("there's no output_hash field on the drv?")
                }

                _ => {}
            }

            // Most of these are also added to the builder's environment in "raw" form.
            if let Some(_) = drv.environment.insert(name.to_string(), val_str) {
                todo!("error: duplicate env in drv yada yada")
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
        .into_iter()
        .map(|builtin| (builtin.name(), Value::Builtin(builtin)))
        .collect()
}
