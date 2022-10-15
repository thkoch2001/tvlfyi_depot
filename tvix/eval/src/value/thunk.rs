//! This module implements the runtime representation of Thunks.
//!
//! Thunks are a special kind of Nix value, similar to a 0-argument
//! closure that yields some value. Thunks are used to implement the
//! lazy evaluation behaviour of Nix:
//!
//! Whenever the compiler determines that an expression should be
//! evaluated lazily, it creates a thunk instead of compiling the
//! expression value directly. At any point in the runtime where the
//! actual value of a thunk is required, it is "forced", meaning that
//! the encompassing computation takes place and the thunk takes on
//! its new value.
//!
//! Thunks have interior mutability to be able to memoise their
//! computation. Once a thunk is evaluated, its internal
//! representation becomes the result of the expression. It is legal
//! for the runtime to replace a thunk object directly with its value
//! object, but when forcing a thunk, the runtime *must* mutate the
//! memoisable slot.

use std::{
    cell::{Ref, RefCell, RefMut},
    fmt::Display,
    ops::Deref,
    rc::Rc,
};

use codemap::Span;

use crate::{
    errors::{Error, ErrorKind},
    upvalues::Upvalues,
    value::Closure,
    vm::VM,
    Value,
};

use super::Lambda;

///
/// Internal representation of the different states of a thunk.
/// The state transition diagram looks like this:
///
///   Suspended -> Blackhole ->--\
///                              |---> Evaluated
///   RecursiveClosure ------->--/
///
/// Upvalues must be finalised before leaving the initial state
/// (Suspended or RecursiveClosure).  The [`value()`] function may
/// not be called until the thunk is in the final state (Evaluated).
///
#[derive(Clone, Debug, PartialEq)]
enum ThunkRepr {
    /// Thunk is closed over some values, suspended and awaiting
    /// execution.
    Suspended {
        lambda: Rc<Lambda>,
        upvalues: Upvalues,
        span: Span,
    },

    /// Thunk currently under-evaluation; encountering a blackhole
    /// value means that infinite recursion has occured.
    Blackhole,

    /// Fully evaluated thunk.
    Evaluated(Value),

    /// A closure which belongs to its own upvalues.
    RecursiveClosure(Closure),
    // TODO(amjoseph): avoid creating `Rc<RefCell<>>` for non-self-referential closures.
}

/// A thunk is created for any value which requires non-strict
/// evaluation due to self-reference or lazy semantics (or both).
/// Every reference cycle involving `Value`s will contain at least
/// one `Thunk`.
#[derive(Clone, Debug, PartialEq)]
pub struct Thunk(Rc<RefCell<ThunkRepr>>);

impl Thunk {
    pub fn new(lambda: Rc<Lambda>, span: Span) -> Self {
        Thunk(Rc::new(RefCell::new(ThunkRepr::Suspended {
            upvalues: Upvalues::with_capacity(lambda.upvalue_count),
            lambda: lambda.clone(),
            span,
        })))
    }

    pub fn new_recursive_closure(c: Closure) -> Self {
        Thunk(Rc::new(RefCell::new(ThunkRepr::RecursiveClosure(c))))
    }

    /// Evaluate the content of a thunk, potentially repeatedly, until
    /// a non-thunk value is returned.
    ///
    /// This will change the existing thunk (and thus all references
    /// to it, providing memoization) through interior mutability. In
    /// case of nested thunks, the intermediate thunk representations
    /// are replaced.
    pub fn force(&self, vm: &mut VM) -> Result<(), ErrorKind> {
        loop {
            let mut thunk_mut = self.0.borrow_mut();

            match *thunk_mut {
                // The purpose of forcing a RecursiveClosure is not to perform
                // computation, but rather to indicate that all upvalues have
                // been fully realised, and that it is therefore safe to call
                // the closure.  Since forcing a RecursiveClosure Thunk carries
                // no risk of nontermination we force them proactively at the
                // earliest opportunity; see [`compiler::emit_upvalue_data()`]
                // and the [`OpFinalise`] branch of [`VM::run_op()`].
                ThunkRepr::RecursiveClosure(_) => {
                    if let ThunkRepr::RecursiveClosure(c) =
                        std::mem::replace(&mut *thunk_mut, ThunkRepr::Blackhole)
                    {
                        // This does an in-place replacement of
                        // RecursiveClosure with Evaluated, without
                        // using clone().
                        drop(thunk_mut);
                        (*self.0.borrow_mut()) = ThunkRepr::Evaluated(Value::Closure(c));
                    }
                }

                ThunkRepr::Evaluated(Value::Thunk(ref inner_thunk)) => {
                    let inner_repr = inner_thunk.0.borrow().clone();
                    *thunk_mut = inner_repr;
                }

                ThunkRepr::Evaluated(_) => return Ok(()),
                ThunkRepr::Blackhole => return Err(ErrorKind::InfiniteRecursion),

                ThunkRepr::Suspended { .. } => {
                    if let ThunkRepr::Suspended {
                        lambda,
                        upvalues,
                        span,
                    } = std::mem::replace(&mut *thunk_mut, ThunkRepr::Blackhole)
                    {
                        drop(thunk_mut);
                        vm.enter_frame(lambda, upvalues, 0)
                            .map_err(|e| ErrorKind::ThunkForce(Box::new(Error { span, ..e })))?;
                        let evaluated = ThunkRepr::Evaluated(vm.pop());
                        (*self.0.borrow_mut()) = evaluated;
                    }
                }
            }
        }
    }

    pub fn is_unforced_recursive_closure(&self) -> bool {
        matches!(
            self.0.as_ref().borrow().deref(),
            ThunkRepr::RecursiveClosure(_)
        )
    }

    /// Returns a reference to the inner evaluated value of a thunk.
    /// It is an error to call this on a thunk that has not been
    /// forced, or is not otherwise known to be fully evaluated.
    // Note: Due to the interior mutability of thunks this is
    // difficult to represent in the type system without impacting the
    // API too much.
    pub fn value(&self) -> Ref<Value> {
        Ref::map(self.0.borrow(), |thunk| {
            if let ThunkRepr::Evaluated(value) = thunk {
                return value;
            }

            panic!("Thunk::value called on non-evaluated thunk");
        })
    }

    pub fn upvalues(&self) -> Ref<'_, Upvalues> {
        Ref::map(self.0.borrow(), |thunk| match thunk {
            ThunkRepr::Suspended { upvalues, .. } => upvalues,
            ThunkRepr::RecursiveClosure(Closure { upvalues, .. }) => upvalues,
            _ => panic!("upvalues() on non-suspended thunk"),
        })
    }

    pub fn upvalues_mut(&self) -> RefMut<'_, Upvalues> {
        RefMut::map(self.0.borrow_mut(), |thunk| match thunk {
            ThunkRepr::Suspended { upvalues, .. } => upvalues,
            ThunkRepr::RecursiveClosure(Closure { upvalues, .. }) => upvalues,
            thunk => panic!("upvalues() on non-suspended thunk: {thunk:?}"),
        })
    }
}

impl Display for Thunk {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.0.try_borrow() {
            Ok(repr) => match &*repr {
                ThunkRepr::Evaluated(v) => v.fmt(f),
                _ => f.write_str("internal[thunk]"),
            },

            _ => f.write_str("internal[thunk]"),
        }
    }
}
