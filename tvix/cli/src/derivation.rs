//! Implements `builtins.derivation`, the core of what makes Nix build packages.

use tvix_eval::{Builtin, BuiltinArgument, CoercionKind, ErrorKind, Value, VM};

// Constants used for strangely named fields in derivation inputs.

const STRUCTURED_ATTRS: &'static str = "__structuredAttrs";
const IGNORE_NULLS: &'static str = "__ignoreNulls";

fn builtins_derivation_strict() -> Builtin {
    let docs = r#"Strictly construct a Nix derivation from the supplied arguments.

Users usually want to use `builtins.derivation` instead."#;

    Builtin::new(
        "derivationStrict",
        &[BuiltinArgument {
            strict: true,
            name: "attrs",
        }],
        Some(docs),
        |args: Vec<Value>, vm: &mut VM| {
            let input = args[0].to_attrs()?;
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
                        drv.platform = val_str.clone();
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
        },
    )
}

fn builtins_derivation() -> Builtin {
    let docs = r#"Construct a Nix derivation from the supplied arguments.

Calling this function immediately causes a build to start."#;

    Builtin::new(
        "derivation",
        &[BuiltinArgument {
            strict: true,
            name: "attrs",
        }],
        Some(docs),
        |_: Vec<Value>, _: &mut VM| Err(ErrorKind::NotImplemented("builtins.derivation")),
    )
}

pub fn derivation_builtins() -> Vec<(&'static str, Value)> {
    vec![
        (
            "derivationStrict",
            Value::Builtin(builtins_derivation_strict()),
        ),
        ("derivation", Value::Builtin(builtins_derivation())),
    ]
}
