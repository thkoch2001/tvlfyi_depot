//! Implements `builtins.derivation`, the core of what makes Nix build packages.

use tvix_eval::{Builtin, BuiltinArgument, ErrorKind, Value, VM};

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

            let name = input.select_required("name")?.to_str()?;

            if let Some(sa) = input.select(STRUCTURED_ATTRS) {
                if sa.as_bool()? {
                    return Err(ErrorKind::NotImplemented(STRUCTURED_ATTRS));
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
        |args: Vec<Value>, vm: &mut VM| Err(ErrorKind::NotImplemented("builtins.derivation")),
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
