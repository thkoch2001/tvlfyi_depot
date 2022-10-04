use std::{
    collections::{BTreeMap, HashMap},
    rc::Rc,
    time::{SystemTime, UNIX_EPOCH},
};

use crate::{
    errors::ErrorKind,
    observer::NoOpObserver,
    value::{Builtin, NixString, Thunk},
    vm::VM,
    SourceCode, Value,
};

fn impure_builtins() -> Vec<Builtin> {
    vec![]
}

/// Return all impure builtins, that is all builtins which may perform I/O
/// outside of the VM and so cannot be used in all contexts (e.g. WASM).
pub(super) fn builtins() -> BTreeMap<NixString, Value> {
    let mut map: BTreeMap<NixString, Value> = impure_builtins()
        .into_iter()
        .map(|b| (b.name().into(), Value::Builtin(b)))
        .collect();

    // currentTime pins the time at which evaluation was started
    {
        let seconds = match SystemTime::now().duration_since(UNIX_EPOCH) {
            Ok(dur) => dur.as_secs() as i64,

            // This case is hit if the system time is *before* epoch.
            Err(err) => -(err.duration().as_secs() as i64),
        };

        map.insert(NixString::from("currentTime"), Value::Integer(seconds));
    }

    map
}

/// Constructs and inserts the `import` builtin. This builtin is special in that
/// it needs to capture the [crate::SourceCode] structure to correctly track
/// source code locations while invoking a compiler.
// TODO: need to be able to pass through a CompilationObserver, too.
pub fn builtins_import(source: SourceCode) -> Builtin {
    Builtin::new(
        "import",
        &[true],
        move |mut args: Vec<Value>, _: &mut VM| {
            let path = match args.pop().unwrap() {
                Value::Path(path) => path,
                Value::String(_) => {
                    return Err(ErrorKind::NotImplemented("importing from string-paths"))
                }
                other => {
                    return Err(ErrorKind::TypeError {
                        expected: "path or string",
                        actual: other.type_of(),
                    })
                }
            };

            let contents =
                std::fs::read_to_string(&path).map_err(|err| ErrorKind::ReadFileError {
                    path: path.clone(),
                    error: Rc::new(err),
                })?;

            let parsed = rnix::ast::Root::parse(&contents);
            let errors = parsed.errors();

            if !errors.is_empty() {
                return Err(ErrorKind::ImportParseError {
                    path,
                    errors: errors.to_vec(),
                });
            }

            let file = source.add_file(path.to_string_lossy().to_string(), contents);

            let result = crate::compile(
                &parsed.tree().expr().unwrap(),
                Some(path.clone()),
                file,
                HashMap::new(), // TODO: pass through globals
                &mut NoOpObserver::default(),
            )
            .map_err(|err| ErrorKind::ImportCompilerError {
                path: path.clone(),
                errors: vec![err],
            })?;

            if !result.errors.is_empty() {
                return Err(ErrorKind::ImportCompilerError {
                    path,
                    errors: result.errors,
                });
            }

            // TODO: deal with runtime *warnings* (most likely through an
            // emit_warning function on the VM that might return it together with
            // the result)

            // Compilation succeeded, we can construct a thunk from whatever it spat
            // out and return that.
            Ok(Value::Thunk(Thunk::new(result.lambda)))
        },
    )
}
