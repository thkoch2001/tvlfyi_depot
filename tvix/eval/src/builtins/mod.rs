//! This module implements the builtins exposed in the Nix language.
//!
//! See //tvix/eval/docs/builtins.md for a some context on the
//! available builtins in Nix.

use std::{
    collections::{BTreeMap, HashMap},
    path::PathBuf,
    rc::Rc,
};

use crate::{
    errors::ErrorKind,
    value::{Builtin, NixAttrs, NixList, NixString, Value},
};

use crate::arithmetic_op;

/// Helper macro to ensure that a value has been forced. The structure
/// of this is a little cumbersome as there are different reference
/// types depending on whether the value is inside a thunk or not.
macro_rules! force {
    ( $vm:ident, $src:expr, $value:ident, $body:block ) => {
        if let Value::Thunk(thunk) = $src {
            thunk.force($vm)?;
            let guard = thunk.value();
            let $value: &Value = &guard;
            $body
        } else {
            let $value: &Value = $src;
            $body
        }
    };

    ( $vm:ident, $value:ident, $body:block ) => {
        force!($vm, &$value, $value, $body)
    };
}

/// Return all pure builtins, that is all builtins that do not rely on
/// I/O outside of the VM and which can be used in any contexts (e.g.
/// WASM).
fn pure_builtins() -> Vec<Builtin> {
    vec![
        Builtin::new("add", 2, |mut args, _| {
            let b = args.pop().unwrap();
            let a = args.pop().unwrap();
            arithmetic_op!(a, b, +)
        }),
        Builtin::new("abort", 1, |mut args, _| {
            return Err(ErrorKind::Abort(
                args.pop().unwrap().to_str()?.as_str().to_owned(),
            ));
        }),
        Builtin::new("attrNames", 1, |args, vm| {
            if let Value::Thunk(t) = &args[0] {
                t.force(vm)?;
            }
            let xs = args[0].to_attrs()?;
            let mut output = vec![];

            for (key, _val) in xs.iter() {
                output.push(Value::String(key.clone()));
            }

            Ok(Value::List(NixList::construct(output.len(), output)))
        }),
        Builtin::new("attrValues", 1, |args, vm| {
            if let Value::Thunk(t) = &args[0] {
                t.force(vm)?;
            }
            let xs = args[0].to_attrs()?;
            let mut output = vec![];

            for (_key, val) in xs.iter() {
                output.push(val.clone());
            }

            Ok(Value::List(NixList::construct(output.len(), output)))
        }),
        Builtin::new("bitAnd", 2, |args, _vm| {
            Ok(Value::Integer(args[0].as_int()? & args[1].as_int()?))
        }),
        Builtin::new("bitOr", 2, |args, _vm| {
            Ok(Value::Integer(args[0].as_int()? | args[1].as_int()?))
        }),
        Builtin::new("bitXor", 2, |args, _vm| {
            Ok(Value::Integer(args[0].as_int()? ^ args[1].as_int()?))
        }),
        Builtin::new("catAttrs", 2, |mut args, _| {
            let list = args.pop().unwrap().to_list()?;
            let key = args.pop().unwrap().to_str()?;
            let mut output = vec![];

            for set in list.into_iter() {
                if let Some(value) = set.to_attrs()?.select(key.as_str()) {
                    output.push(value.clone());
                }
            }

            Ok(Value::List(NixList::construct(output.len(), output)))
        }),
        Builtin::new("div", 2, |mut args, _| {
            let b = args.pop().unwrap();
            let a = args.pop().unwrap();
            arithmetic_op!(a, b, /)
        }),
        Builtin::new("elemAt", 2, |args, vm| {
            if let Value::Thunk(t) = &args[0] {
                t.force(vm)?;
            }
            let xs = args[0].to_list()?;
            let i = args[1].as_int()? as usize;
            match xs.get(i) {
                Some(x) => Ok(x.clone()),
                None => Err(ErrorKind::Abort(format!(
                    "list index {} is out of bounds",
                    i
                ))),
            }
        }),
        Builtin::new("getAttr", 2, |args, vm| {
            if let Value::Thunk(t) = &args[1] {
                t.force(vm)?;
            }
            let k = args[0].to_str()?;
            let xs = args[1].to_attrs()?;

            match xs.select(k.as_str()) {
                Some(x) => Ok(x.clone()),
                None => Err(ErrorKind::Abort(format!("attribute '{}' missing", k))),
            }
        }),
        Builtin::new("length", 1, |args, vm| {
            if let Value::Thunk(t) = &args[0] {
                t.force(vm)?;
            }
            Ok(Value::Integer(args[0].to_list()?.len() as i64))
        }),
        Builtin::new("hasAttr", 2, |args, vm| {
            if let Value::Thunk(t) = &args[1] {
                t.force(vm)?;
            }
            let k = args[0].to_str()?;
            let xs = args[1].to_attrs()?;

            Ok(Value::Bool(xs.contains(k.as_str())))
        }),
        Builtin::new("head", 1, |args, vm| {
            if let Value::Thunk(t) = &args[0] {
                t.force(vm)?;
            }
            let xs = args[0].to_list()?;
            match xs.get(0) {
                Some(x) => Ok(x.clone()),
                None => Err(ErrorKind::Abort(
                    "list index 0 is out of bounds".to_string(),
                )),
            }
        }),
        Builtin::new("isAttrs", 1, |args, _| {
            Ok(Value::Bool(matches!(args[0], Value::Attrs(_))))
        }),
        Builtin::new("isBool", 1, |args, _| {
            Ok(Value::Bool(matches!(args[0], Value::Bool(_))))
        }),
        Builtin::new("isFloat", 1, |args, _| {
            Ok(Value::Bool(matches!(args[0], Value::Float(_))))
        }),
        Builtin::new("isFunction", 1, |args, _| {
            Ok(Value::Bool(matches!(
                args[0],
                Value::Closure(_) | Value::Builtin(_)
            )))
        }),
        Builtin::new("isInt", 1, |args, _| {
            Ok(Value::Bool(matches!(args[0], Value::Integer(_))))
        }),
        Builtin::new("isList", 1, |args, _| {
            Ok(Value::Bool(matches!(args[0], Value::List(_))))
        }),
        Builtin::new("isNull", 1, |args, _| {
            Ok(Value::Bool(matches!(args[0], Value::Null)))
        }),
        Builtin::new("isPath", 1, |args, _| {
            Ok(Value::Bool(matches!(args[0], Value::Path(_))))
        }),
        Builtin::new("isString", 1, |args, _| {
            Ok(Value::Bool(matches!(args[0], Value::String(_))))
        }),
        Builtin::new("mul", 2, |mut args, _| {
            let b = args.pop().unwrap();
            let a = args.pop().unwrap();
            arithmetic_op!(a, b, *)
        }),
        Builtin::new("sub", 2, |mut args, _| {
            let b = args.pop().unwrap();
            let a = args.pop().unwrap();
            arithmetic_op!(a, b, -)
        }),
        Builtin::new("tail", 1, |args, vm| {
            if let Value::Thunk(t) = &args[0] {
                t.force(vm)?;
            }
            let xs = args[0].to_list()?;
            if xs.len() == 0 {
                Err(ErrorKind::Abort(
                    "'tail' called on an empty list".to_string(),
                ))
            } else {
                let mut output = vec![];
                for x in args[0].to_list()?.iter().skip(1) {
                    output.push(x.clone());
                }
                Ok(Value::List(NixList::construct(output.len(), output)))
            }
        }),
        Builtin::new("throw", 1, |mut args, _| {
            return Err(ErrorKind::Throw(
                args.pop().unwrap().to_str()?.as_str().to_owned(),
            ));
        }),
        Builtin::new("toString", 1, |args, vm| {
            force!(vm, &args[0], value, {
                Ok(Value::String(format!("{}", value).into()))
            })
        }),
        Builtin::new("typeOf", 1, |args, _| {
            Ok(Value::String(args[0].type_of().into()))
        }),
    ]
}

// Return all impure builtins, which are builtins that rely on I/O and may not
// be suitable for environments like WASM.
fn impure_builtins() -> Vec<Builtin> {
    vec![
        Builtin::new("readDir", 1, |args, _| {
            let do_read_dir = |x: &PathBuf| {
                let mut stack_slice: Vec<Value> = vec![];
                for entry in x.read_dir()? {
                    if let Ok(entry) = entry {
                        let file_type = entry.metadata().unwrap().file_type();
                        let val = if file_type.is_dir() {
                            "directory"
                        } else if file_type.is_file() {
                            "regular"
                        } else {
                            "symlink"
                        };
                        stack_slice.push(Value::String(entry.file_name().to_str().unwrap().into()));
                        stack_slice.push(Value::String(val.into()));
                    }
                }
                Ok(Value::Attrs(Rc::new(NixAttrs::construct(
                    stack_slice.len() / 2,
                    stack_slice,
                )?)))
            };

            match &args[0] {
                Value::String(x) => do_read_dir(&x.as_str().to_string().into()),
                Value::Path(x) => do_read_dir(x),
                x => Err(ErrorKind::Abort(format!(
                    "cannot coerce a {} to a string",
                    x.type_of()
                ))),
            }
        }),
    ]
}

fn builtins_set() -> NixAttrs {
    let mut map: BTreeMap<NixString, Value> = BTreeMap::new();

    for builtin in pure_builtins() {
        map.insert(builtin.name().into(), Value::Builtin(builtin));
    }

    for builtin in impure_builtins() {
        map.insert(builtin.name().into(), Value::Builtin(builtin));
    }

    NixAttrs::from_map(map)
}

/// Set of Nix builtins that are globally available.
pub fn global_builtins() -> HashMap<&'static str, Value> {
    let builtins = builtins_set();
    let mut globals: HashMap<&'static str, Value> = HashMap::new();

    // known global builtins from the builtins set.
    for global in &[
        "abort",
        "baseNameOf",
        "derivation",
        "derivationStrict",
        "dirOf",
        "fetchGit",
        "fetchMercurial",
        "fetchTarball",
        "fromTOML",
        "import",
        "isNull",
        "map",
        "placeholder",
        "removeAttrs",
        "scopedImport",
        "throw",
        "toString",
    ] {
        if let Some(builtin) = builtins.select(global) {
            globals.insert(global, builtin.clone());
        }
    }

    globals.insert("builtins", Value::Attrs(Rc::new(builtins)));

    globals
}
