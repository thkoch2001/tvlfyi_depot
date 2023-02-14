//! This module implements the runtime representation of a Nix
//! builtin.
//!
//! Builtins are directly backed by Rust code operating on Nix values.

use crate::vm::generators::Generator;
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
pub trait BuiltinFn: Fn(Vec<Value>, &mut VM) -> Result<Value, ErrorKind> {}
impl<F: Fn(Vec<Value>, &mut VM) -> Result<Value, ErrorKind>> BuiltinFn for F {}

/// Description of a single argument passed to a builtin
pub struct BuiltinArgument {
    /// Whether the argument should be forced before the underlying builtin function is called
    pub strict: bool,
    /// The name of the argument, to be used in docstrings and error messages
    pub name: &'static str,
}

/// Trait for closure types of builtins.
///
/// Builtins are expected to yield a generator which can be run by the VM to
/// produce the final value.
///
/// Implementors should use the builtins-macros to create these functions
/// instead of handling the argument-passing logic manually.
pub trait BuiltinGen: Fn(Vec<Value>) -> Generator {}
impl<F: Fn(Vec<Value>) -> Generator> BuiltinGen for F {}

#[derive(Clone)]
pub struct BuiltinRepr {
    name: &'static str,
    /// Array of arguments to the builtin.
    arguments: &'static [BuiltinArgument],
    /// Optional documentation for the builtin.
    documentation: Option<&'static str>,
    func: Rc<dyn BuiltinGen>,

    /// Partially applied function arguments.
    partials: Vec<Value>,
}

pub enum BuiltinResult {
    /// Builtin was not ready to be called (arguments missing) and remains
    /// partially applied.
    Partial(Builtin),

    /// Builtin was called and constructed a generator that the VM must run.
    Called(Generator),
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
pub struct Builtin(Box<BuiltinRepr>);

impl From<BuiltinRepr> for Builtin {
    fn from(value: BuiltinRepr) -> Self {
        Builtin(Box::new(value))
    }
}

impl Builtin {
    pub fn new<F: BuiltinGen + 'static>(
        name: &'static str,
        arguments: &'static [BuiltinArgument],
        documentation: Option<&'static str>,
        func: F,
    ) -> Self {
        BuiltinRepr {
            name,
            arguments,
            documentation,
            func: Rc::new(func),
            partials: vec![],
        }
        .into()
    }

    pub fn name(&self) -> &'static str {
        self.0.name
    }

    pub fn documentation(&self) -> Option<&'static str> {
        self.0.documentation
    }

    /// Apply an additional argument to the builtin. After this, [`call`] *must*
    /// be called, otherwise it may leave the builtin in an incorrect state.
    pub fn apply_arg(&mut self, arg: Value) {
        self.0.partials.push(arg);

        debug_assert!(
            self.0.partials.len() <= self.0.arguments.len(),
            "Tvix bug: pushed too many arguments to builtin"
        );
    }

    /// Attempt to call a builtin, which will produce a generator if it is fully
    /// applied or return the builtin if it is partially applied.
    pub fn call(self) -> BuiltinResult {
        if self.0.partials.len() == self.0.arguments.len() {
            return BuiltinResult::Called((self.0.func)(self.0.partials));
        }

        BuiltinResult::Partial(self)
    }
}

impl Debug for Builtin {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "builtin[{}]", self.0.name)
    }
}

impl Display for Builtin {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if !self.0.partials.is_empty() {
            f.write_str("<<primop-app>>")
        } else {
            f.write_str("<<primop>>")
        }
    }
}

/// Builtins are uniquely identified by their name
impl PartialEq for Builtin {
    fn eq(&self, other: &Self) -> bool {
        self.0.name == other.0.name
    }
}
