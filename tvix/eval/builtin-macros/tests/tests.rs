use tvix_eval_builtin_macros::builtins;

mod value {
    pub use tvix_eval::Builtin;
}

#[builtins]
mod builtins {
    use tvix_eval::{ErrorKind, Value, VM};

    #[builtin("identity", [true])]
    pub fn builtin_identity(_vm: &mut VM, x: Value) -> Result<Value, ErrorKind> {
        Ok(x)
    }
}

#[test]
fn builtins() {
    let builtins = builtins::builtins();
    assert_eq!(builtins.len(), 1);
}
