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
    cell::{Ref, RefCell},
    fmt::Display,
    rc::Rc,
};

use codemap::Span;

use crate::{
    errors::{Error, ErrorKind},
    upvalues::Upvalues,
    value::function::InnerClosure,
    vm::VM,
    Value,
};

use super::Lambda;

/// Internal representation of the different states of a thunk.
#[derive(Clone, Debug, PartialEq)]
pub enum ThunkRepr {
    /// Thunk is closed over some values, suspended and awaiting
    /// execution.
    Suspended(InnerClosure),

    /// Thunk currently under-evaluation; encountering a blackhole
    /// value means that infinite recursion has occured.
    Blackhole,

    /// Fully evaluated thunk.
    Evaluated(Value),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Thunk {
    inner: Rc<RefCell<ThunkRepr>>,
    span: Span,
}

impl Thunk {
    pub fn new(lambda: Rc<Lambda>, span: Span) -> Self {
        Thunk {
            inner: Rc::new(RefCell::new(ThunkRepr::Suspended(InnerClosure {
                upvalues: Upvalues::with_capacity(lambda.upvalue_count),
                lambda: lambda.clone(),
            }))),
            span,
        }
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
            let mut thunk_mut = self.inner.borrow_mut();

            match *thunk_mut {
                ThunkRepr::Evaluated(Value::Thunk(ref inner_thunk)) => {
                    let inner_repr = inner_thunk.inner.borrow().clone();
                    *thunk_mut = inner_repr;
                }

                ThunkRepr::Evaluated(_) => return Ok(()),
                ThunkRepr::Blackhole => return Err(ErrorKind::InfiniteRecursion),

                ThunkRepr::Suspended { .. } => {
                    if let ThunkRepr::Suspended(InnerClosure { lambda, upvalues }) =
                        std::mem::replace(&mut *thunk_mut, ThunkRepr::Blackhole)
                    {
                        drop(thunk_mut);
                        vm.enter_frame(lambda, upvalues, 0).map_err(|e| {
                            ErrorKind::ThunkForce(Box::new(Error {
                                span: self.span,
                                ..e
                            }))
                        })?;
                        let evaluated = ThunkRepr::Evaluated(vm.pop());
                        (*self.inner.borrow_mut()) = evaluated;
                    }
                }
            }
        }
    }

    /// Returns a reference to the inner evaluated value of a thunk.
    /// It is an error to call this on a thunk that has not been
    /// forced, or is not otherwise known to be fully evaluated.
    // Note: Due to the interior mutability of thunks this is
    // difficult to represent in the type system without impacting the
    // API too much.
    pub fn value(&self) -> Ref<Value> {
        Ref::map(self.inner.borrow(), |thunk| {
            if let ThunkRepr::Evaluated(value) = thunk {
                return value;
            }

            panic!("Thunk::value called on non-evaluated thunk");
        })
    }
}

impl AsRef<Rc<RefCell<ThunkRepr>>> for Thunk {
    fn as_ref(&self) -> &Rc<RefCell<ThunkRepr>> {
        &self.inner
    }
}

impl AsMut<Upvalues> for ThunkRepr {
    fn as_mut(&mut self) -> &mut Upvalues {
        if let ThunkRepr::Suspended(ic) = self {
            &mut ic.upvalues
        } else {
            panic!("upvalues() on non-suspended thunk");
        }
    }
}

impl AsRef<InnerClosure> for ThunkRepr {
    fn as_ref(&self) -> &InnerClosure {
        if let ThunkRepr::Suspended(ic) = self {
            &ic
        } else {
            panic!("upvalues() on non-suspended thunk");
        }
    }
}

impl Display for Thunk {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.inner.try_borrow() {
            Ok(repr) => match &*repr {
                ThunkRepr::Evaluated(v) => v.fmt(f),
                _ => f.write_str("internal[thunk]"),
            },

            _ => f.write_str("internal[thunk]"),
        }
    }
}
