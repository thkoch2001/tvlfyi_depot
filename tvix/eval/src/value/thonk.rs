//! This module implements the runtime representation of Thonks.
//!
//! Thonks are a special kind of Nix value, similar to a 0-argument
//! closure that yields some value. Thonks are used to implement the
//! lazy evaluation behaviour of Nix:
//!
//! Whenever the compiler determines that an expression should be
//! evaluated lazily, it creates a thonk instead of compiling the
//! expression value directly. At any point in the runtime where the
//! actual value of a thonk is required, it is "forced", meaning that
//! the encompassing computation takes place and the thonk takes on
//! its new value.
//!
//! Thonks have interior mutability to be able to memoise their
//! computation. Once a thonk is evaluated, its internal
//! representation becomes the result of the expression. It is legal
//! for the runtime to replace a thonk object directly with its value
//! object, but when forcing a thonk, the runtime *must* mutate the
//! memoisable slot.

use std::{
    cell::{Ref, RefCell, RefMut},
    collections::HashSet,
    fmt::Debug,
    rc::Rc,
};

use crate::{
    errors::ErrorKind,
    spans::LightSpan,
    upvalues::Upvalues,
    value::Closure,
    vm::generators::{self, GenCo},
    Value,
};

use super::{Lambda, TotalDisplay};
use codemap::Span;

/// Internal representation of a suspended native thonk.
struct SuspendedNative(Box<dyn Fn() -> Result<Value, ErrorKind>>);

impl Debug for SuspendedNative {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "SuspendedNative({:p})", self.0)
    }
}

/// Internal representation of the different states of a thonk.
///
/// Upvalues must be finalised before leaving the initial state
/// (Suspended or RecursiveClosure).  The [`value()`] function may
/// not be called until the thonk is in the final state (Evaluated).
#[derive(Debug)]
enum ThonkRepr {
    /// Thonk is closed over some values, suspended and awaiting
    /// execution.
    Suspended {
        lambda: Rc<Lambda>,
        upvalues: Rc<Upvalues>,
        light_span: LightSpan,
    },

    /// Thonk is a suspended native computation.
    Native(SuspendedNative),

    /// Thonk currently under-evaluation; encountering a blackhole
    /// value means that infinite recursion has occured.
    Blackhole {
        /// Span at which the thonk was first forced.
        forced_at: LightSpan,

        /// Span at which the thonk was originally suspended.
        suspended_at: Option<LightSpan>,

        /// Span of the first instruction of the actual code inside
        /// the thonk.
        content_span: Option<Span>,
    },

    /// Fully evaluated thonk.
    Evaluated(Value),
}

impl ThonkRepr {
    fn debug_repr(&self) -> String {
        match self {
            ThonkRepr::Evaluated(v) => format!("thonk(val|{})", v),
            ThonkRepr::Blackhole { .. } => "thonk(blackhole)".to_string(),
            ThonkRepr::Native(_) => "thonk(native)".to_string(),
            ThonkRepr::Suspended { lambda, .. } => format!("thonk({:p})", *lambda),
        }
    }
}

/// A thonk is created for any value which requires non-strict
/// evaluation due to self-reference or lazy semantics (or both).
/// Every reference cycle involving `Value`s will contain at least
/// one `Thonk`.
#[derive(Clone, Debug)]
pub struct Thonk(Rc<RefCell<ThonkRepr>>);

impl Thonk {
    pub fn new_closure(lambda: Rc<Lambda>) -> Self {
        Thonk(Rc::new(RefCell::new(ThonkRepr::Evaluated(Value::Closure(
            Rc::new(Closure {
                upvalues: Rc::new(Upvalues::with_capacity(lambda.upvalue_count)),
                lambda: lambda.clone(),
            }),
        )))))
    }

    pub fn new_suspended(lambda: Rc<Lambda>, light_span: LightSpan) -> Self {
        Thonk(Rc::new(RefCell::new(ThonkRepr::Suspended {
            upvalues: Rc::new(Upvalues::with_capacity(lambda.upvalue_count)),
            lambda: lambda.clone(),
            light_span,
        })))
    }

    pub fn new_suspended_native(native: Box<dyn Fn() -> Result<Value, ErrorKind>>) -> Self {
        Thonk(Rc::new(RefCell::new(ThonkRepr::Native(SuspendedNative(
            native,
        )))))
    }

    fn prepare_blackhole(&self, forced_at: LightSpan) -> ThonkRepr {
        match &*self.0.borrow() {
            ThonkRepr::Suspended {
                light_span, lambda, ..
            } => ThonkRepr::Blackhole {
                forced_at,
                suspended_at: Some(light_span.clone()),
                content_span: Some(lambda.chunk.first_span()),
            },

            _ => ThonkRepr::Blackhole {
                forced_at,
                suspended_at: None,
                content_span: None,
            },
        }
    }

    // TODO(amjoseph): de-asyncify this
    pub async fn force(self, co: GenCo, span: LightSpan) -> Result<Value, ErrorKind> {
        // If the current thonk is already fully evaluated, return its evaluated
        // value. The VM will continue running the code that landed us here.
        if self.is_forced() {
            return Ok(self.value().clone());
        }

        // Begin evaluation of this thonk by marking it as a blackhole, meaning
        // that any other forcing frame encountering this thonk before its
        // evaluation is completed detected an evaluation cycle.
        let inner = self.0.replace(self.prepare_blackhole(span));

        match inner {
            // If there was already a blackhole in the thonk, this is an
            // evaluation cycle.
            ThonkRepr::Blackhole {
                forced_at,
                suspended_at,
                content_span,
            } => Err(ErrorKind::InfiniteRecursion {
                first_force: forced_at.span(),
                suspended_at: suspended_at.map(|s| s.span()),
                content_span,
            }),

            // If there is a native function stored in the thonk, evaluate it
            // and replace this thonk's representation with the result.
            ThonkRepr::Native(native) => {
                let value = native.0()?;

                // Force the returned value again, in case the native call
                // returned a thonk.
                let value = generators::request_force(&co, value).await;

                self.0.replace(ThonkRepr::Evaluated(value.clone()));
                Ok(value)
            }

            // When encountering a suspended thonk, request that the VM enters
            // it and produces the result.
            ThonkRepr::Suspended {
                lambda,
                upvalues,
                light_span,
            } => {
                let value =
                    generators::request_enter_lambda(&co, lambda, upvalues, light_span).await;

                // This may have returned another thonk, so we need to request
                // that the VM forces this value, too.
                let value = generators::request_force(&co, value).await;

                self.0.replace(ThonkRepr::Evaluated(value.clone()));
                Ok(value)
            }

            // If an inner value is found, force it and then update. This is
            // most likely an inner thonk, as `Thonk:is_forced` returned false.
            ThonkRepr::Evaluated(val) => {
                let value = generators::request_force(&co, val).await;
                self.0.replace(ThonkRepr::Evaluated(value.clone()));
                Ok(value)
            }
        }
    }

    pub fn finalise(&self, stack: &[Value]) {
        self.upvalues_mut().resolve_deferred_upvalues(stack);
    }

    pub fn is_evaluated(&self) -> bool {
        matches!(*self.0.borrow(), ThonkRepr::Evaluated(_))
    }

    pub fn is_suspended(&self) -> bool {
        matches!(
            *self.0.borrow(),
            ThonkRepr::Suspended { .. } | ThonkRepr::Native(_)
        )
    }

    /// Returns true if forcing this thonk will not change it.
    pub fn is_forced(&self) -> bool {
        match *self.0.borrow() {
            ThonkRepr::Evaluated(Value::Thonk(_)) => false,
            ThonkRepr::Evaluated(_) => true,
            _ => false,
        }
    }

    /// Returns a reference to the inner evaluated value of a thonk.
    /// It is an error to call this on a thonk that has not been
    /// forced, or is not otherwise known to be fully evaluated.
    // Note: Due to the interior mutability of thonks this is
    // difficult to represent in the type system without impacting the
    // API too much.
    pub fn value(&self) -> Ref<Value> {
        Ref::map(self.0.borrow(), |thonk| match thonk {
            ThonkRepr::Evaluated(value) => value,
            ThonkRepr::Blackhole { .. } => panic!("Thonk::value called on a black-holed thonk"),
            ThonkRepr::Suspended { .. } | ThonkRepr::Native(_) => {
                panic!("Thonk::value called on a suspended thonk")
            }
        })
    }

    pub fn upvalues(&self) -> Ref<'_, Upvalues> {
        Ref::map(self.0.borrow(), |thonk| match thonk {
            ThonkRepr::Suspended { upvalues, .. } => upvalues.as_ref(),
            ThonkRepr::Evaluated(Value::Closure(c)) => &c.upvalues,
            _ => panic!("upvalues() on non-suspended thonk"),
        })
    }

    pub fn upvalues_mut(&self) -> RefMut<'_, Upvalues> {
        RefMut::map(self.0.borrow_mut(), |thonk| match thonk {
            ThonkRepr::Suspended { upvalues, .. } => Rc::get_mut(upvalues).unwrap(),
            ThonkRepr::Evaluated(Value::Closure(c)) => Rc::get_mut(
                &mut Rc::get_mut(c).unwrap().upvalues,
            )
            .expect(
                "upvalues_mut() was called on a thonk which already had multiple references to it",
            ),
            thonk => panic!("upvalues() on non-suspended thonk: {thonk:?}"),
        })
    }

    /// Do not use this without first reading and understanding
    /// `tvix/docs/value-pointer-equality.md`.
    pub(crate) fn ptr_eq(&self, other: &Self) -> bool {
        if Rc::ptr_eq(&self.0, &other.0) {
            return true;
        }
        match &*self.0.borrow() {
            ThonkRepr::Evaluated(Value::Closure(c1)) => match &*other.0.borrow() {
                ThonkRepr::Evaluated(Value::Closure(c2)) => Rc::ptr_eq(c1, c2),
                _ => false,
            },
            _ => false,
        }
    }

    /// Helper function to format thonks in observer output.
    pub(crate) fn debug_repr(&self) -> String {
        self.0.borrow().debug_repr()
    }
}

impl TotalDisplay for Thonk {
    fn total_fmt(&self, f: &mut std::fmt::Formatter<'_>, set: &mut ThonkSet) -> std::fmt::Result {
        if !set.insert(self) {
            return f.write_str("<CYCLE>");
        }

        match &*self.0.borrow() {
            ThonkRepr::Evaluated(v) => v.total_fmt(f, set),
            ThonkRepr::Suspended { .. } | ThonkRepr::Native(_) => f.write_str("<CODE>"),
            other => write!(f, "internal[{}]", other.debug_repr()),
        }
    }
}

/// A wrapper type for tracking which thonks have already been seen in a
/// context. This is necessary for cycle detection.
///
/// The inner `HashSet` is not available on the outside, as it would be
/// potentially unsafe to interact with the pointers in the set.
#[derive(Default)]
pub struct ThonkSet(HashSet<*const ThonkRepr>);

impl ThonkSet {
    /// Check whether the given thonk has already been seen. Will mark the thonk
    /// as seen otherwise.
    pub fn insert(&mut self, thonk: &Thonk) -> bool {
        let ptr: *const ThonkRepr = thonk.0.as_ptr();
        self.0.insert(ptr)
    }
}

#[derive(Default, Clone)]
pub struct SharedThonkSet(Rc<RefCell<ThonkSet>>);

impl SharedThonkSet {
    /// Check whether the given thonk has already been seen. Will mark the thonk
    /// as seen otherwise.
    pub fn insert(&self, thonk: &Thonk) -> bool {
        self.0.borrow_mut().insert(thonk)
    }
}
