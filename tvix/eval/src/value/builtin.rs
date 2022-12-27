//! This module implements the runtime representation of a Nix
//! builtin.
//!
//! Builtins are directly backed by Rust code operating on Nix values.

use crate::{errors::ErrorKind, vm::VM};

use super::Value;

use std::{
    fmt::{Debug, Display},
    rc::Rc,
};

/// Trait for closure types of builtins implemented directly by
/// backing Rust code.
///
/// Builtins declare their arity and are passed a vector with the
/// right number of arguments. Additionally, as they might have to
/// force the evaluation of thunks, they are passed a reference to the
/// current VM which they can use for forcing a value.
///
/// Errors returned from a builtin will be annotated with the location
/// of the call to the builtin.
pub trait BuiltinFn<RO>: Fn(Vec<Value<RO>>, &mut VM<RO>) -> Result<Value<RO>, ErrorKind> {}
impl<RO, F: Fn(Vec<Value<RO>>, &mut VM<RO>) -> Result<Value<RO>, ErrorKind>> BuiltinFn<RO> for F {}

/// Description of a single argument passed to a builtin
pub struct BuiltinArgument {
    /// Whether the argument should be forced before the underlying builtin function is called
    pub strict: bool,
    /// The name of the argument, to be used in docstrings and error messages
    pub name: &'static str,
}

/// Represents a single built-in function which directly executes Rust
/// code that operates on a Nix value.
///
/// Builtins are the only functions in Nix that have varying arities
/// (for example, `hasAttr` has an arity of 2, but `isAttrs` an arity
/// of 1). To facilitate this generically, builtins expect to be
/// called with a vector of Nix values corresponding to their
/// arguments in order.
///
/// Partially applied builtins act similar to closures in that they
/// "capture" the partially applied arguments, and are treated
/// specially when printing their representation etc.
#[derive(Clone)]
pub struct Builtin<RO> {
    name: &'static str,
    /// Array of arguments to the builtin.
    arguments: &'static [BuiltinArgument],
    /// Optional documentation for the builtin.
    documentation: Option<&'static str>,
    func: Rc<dyn BuiltinFn<RO>>,

    /// Partially applied function arguments.
    partials: Vec<Value<RO>>,
}

impl<RO> Builtin<RO> {
    pub fn new<F: BuiltinFn<RO> + 'static>(
        name: &'static str,
        arguments: &'static [BuiltinArgument],
        documentation: Option<&'static str>,
        func: F,
    ) -> Self {
        Builtin {
            name,
            arguments,
            documentation,
            func: Rc::new(func),
            partials: vec![],
        }
    }

    pub fn name(&self) -> &'static str {
        self.name
    }

    pub fn documentation(&self) -> Option<&'static str> {
        self.documentation
    }

    /// Apply an additional argument to the builtin, which will either
    /// lead to execution of the function or to returning a partial
    /// builtin.
    pub fn apply(mut self, vm: &mut VM<RO>, arg: Value<RO>) -> Result<Value<RO>, ErrorKind> {
        self.partials.push(arg);

        if self.partials.len() == self.arguments.len() {
            for (idx, BuiltinArgument { strict, .. }) in self.arguments.iter().enumerate() {
                if *strict {
                    self.partials[idx].force(vm)?;
                }
            }
            return (self.func)(self.partials, vm);
        }

        // Function is not yet ready to be called.
        Ok(Value::Builtin(self))
    }
}

impl<RO> Debug for Builtin<RO> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "builtin[{}]", self.name)
    }
}

impl<RO> Display for Builtin<RO> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if !self.partials.is_empty() {
            f.write_str("<<primop-app>>")
        } else {
            f.write_str("<<primop>>")
        }
    }
}

/// Builtins are uniquely identified by their name
impl<RO> PartialEq for Builtin<RO> {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}
