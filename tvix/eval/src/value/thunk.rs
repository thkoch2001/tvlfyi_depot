#![allow(unused-variables)]
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
    collections::HashSet,
    fmt::Debug,
    rc::Rc,
};

use crate::{
    errors::ErrorKind,
    opcode::OpCode,
    spans::LightSpan,
    upvalues::Upvalues,
    value::Closure,
    vm::generators::{self, GenCo},
    Value,
};

use super::{Lambda, TotalDisplay};
use codemap::Span;

/// Internal representation of a suspended native thunk.
struct SuspendedNative(Box<dyn Fn() -> Result<Value, ErrorKind>>);

impl Debug for SuspendedNative {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "SuspendedNative({:p})", self.0)
    }
}

/// Internal representation of the different states of a thunk.
///
/// Upvalues must be finalised before leaving the initial state
/// (Suspended or RecursiveClosure).  The [`value()`] function may
/// not be called until the thunk is in the final state (Evaluated).
#[derive(Debug)]
enum ThunkRepr {
    /// Thunk is closed over some values, suspended and awaiting
    /// execution.
    Suspended {
        lambda: Rc<Lambda>,
        upvalues: Rc<Upvalues>,
        light_span: LightSpan,
    },

    /// Thunk is a suspended native computation.
    Native(SuspendedNative),

    /// Thunk currently under-evaluation; encountering a blackhole
    /// value means that infinite recursion has occured.
    Blackhole {
        /// Span at which the thunk was first forced.
        forced_at: LightSpan,

        /// Span at which the thunk was originally suspended.
        suspended_at: Option<LightSpan>,

        /// Span of the first instruction of the actual code inside
        /// the thunk.
        content_span: Option<Span>,

        /// During thunk-forcing, if we encounter a long chain of
        /// thunks we will turn them into a long chain of blackholes
        /// (with the order reversed); this field prev_ptr is used
        /// to link the blackholes together.  Once we get to the
        /// final thunk in the chain and finish forcing it, we walk
        /// the blackhole chain and update all the thunks with
        /// copies of the final value.
        prev_ptr: Option<Rc<RefCell<ThunkRepr>>>,
    },

    // TODO(amjoseph): consider changing `Value` to `Rc<Value>` to avoid
    // expensive clone()s in Thunk::force().
    /// Fully evaluated thunk.
    Evaluated(Value, ForcingDepth),
}

/// Indicates how deeply a thunk has been forced.
#[derive(Clone, Copy, Debug)]
pub enum ForcingDepth {
    /// The thunk has *already* been shallowly forced.
    Shallowly,

    /// The thunk has *already* been shallowly forced, and is either
    /// deeply forced *or in the process of being deeply forced*.
    Deeply,
}

impl ThunkRepr {
    fn debug_repr(&self) -> String {
        match self {
            ThunkRepr::Evaluated(v, _) => format!("thunk(val|{})", v),
            ThunkRepr::Blackhole { .. } => "thunk(blackhole)".to_string(),
            ThunkRepr::Native(_) => "thunk(native)".to_string(),
            ThunkRepr::Suspended { lambda, .. } => format!("thunk({:p})", *lambda),
        }
    }

    /// Return the Value within a fully-evaluated ThunkRepr; panics
    /// if the thunk is not fully-evaluated.
    fn expect(self) -> Value {
        match self {
            ThunkRepr::Evaluated(value, _) => value,
            ThunkRepr::Blackhole { .. } => panic!("Thunk::expect() called on a black-holed thunk"),
            ThunkRepr::Suspended { .. } | ThunkRepr::Native(_) => {
                panic!("Thunk::expect() called on a suspended thunk")
            }
        }
    }

    fn expect_ref(&self) -> &Value {
        match self {
            ThunkRepr::Evaluated(value, _) => value,
            ThunkRepr::Blackhole { .. } => panic!("Thunk::expect() called on a black-holed thunk"),
            ThunkRepr::Suspended { .. } | ThunkRepr::Native(_) => {
                panic!("Thunk::expect() called on a suspended thunk")
            }
        }
    }

    pub fn is_forced(&self) -> bool {
        match self {
            ThunkRepr::Evaluated(Value::Thunk(_), _) => false,
            ThunkRepr::Evaluated(_, _) => true,
            _ => false,
        }
    }

    pub fn get_forcing_depth(&self) -> Option<ForcingDepth> {
        match self {
            ThunkRepr::Evaluated(Value::Thunk(_), _) => None,
            ThunkRepr::Evaluated(_, depth) => Some(*depth),
            _ => None,
        }
    }
}

/// A thunk is created for any value which requires non-strict
/// evaluation due to self-reference or lazy semantics (or both).
/// Every reference cycle involving `Value`s will contain at least
/// one `Thunk`.
#[derive(Clone, Debug)]
pub struct Thunk(Rc<RefCell<ThunkRepr>>);

impl Thunk {
    pub fn get_forcing_depth(&self) -> Option<ForcingDepth> {
        self.0.borrow().get_forcing_depth()
    }

    /// You must only call this if you are currently in the process
    /// of performing a deep-force.
    pub fn set_forcing_depth_deeply(&self, span: LightSpan) {
        let inner = self.0.replace(self.prepare_blackhole(span, None));
        let value = inner.expect();
        self.0
            .replace(ThunkRepr::Evaluated(value, ForcingDepth::Deeply));
    }

    pub fn new_closure(lambda: Rc<Lambda>) -> Self {
        Thunk(Rc::new(RefCell::new(ThunkRepr::Evaluated(
            Value::Closure(Rc::new(Closure {
                upvalues: Rc::new(Upvalues::with_capacity(lambda.upvalue_count)),
                lambda: lambda.clone(),
            })),
            ForcingDepth::Deeply,
        ))))
    }

    pub fn new_suspended(lambda: Rc<Lambda>, light_span: LightSpan) -> Self {
        Thunk(Rc::new(RefCell::new(ThunkRepr::Suspended {
            upvalues: Rc::new(Upvalues::with_capacity(lambda.upvalue_count)),
            lambda: lambda.clone(),
            light_span,
        })))
    }

    pub fn new_suspended_native(native: Box<dyn Fn() -> Result<Value, ErrorKind>>) -> Self {
        Thunk(Rc::new(RefCell::new(ThunkRepr::Native(SuspendedNative(
            native,
        )))))
    }

    /// Helper function to create a [`Thunk`] that calls a function given as the
    /// [`Value`] `callee` with the argument `arg` when it is forced. This is
    /// particularly useful in builtin implementations if the result of calling
    /// a function does not need to be forced immediately, because e.g. it is
    /// stored in an attribute set.
    pub fn new_suspended_call(callee: Value, arg: Value, light_span: LightSpan) -> Self {
        let mut lambda = Lambda::default();
        let span = light_span.span();

        let arg_idx = lambda.chunk().push_constant(arg);
        let f_idx = lambda.chunk().push_constant(callee);

        // This is basically a recreation of compile_apply():
        // We need to push the argument onto the stack and then the function.
        // The function (not the argument) needs to be forced before calling.
        lambda.chunk.push_op(OpCode::OpConstant(arg_idx), span);
        lambda.chunk().push_op(OpCode::OpConstant(f_idx), span);
        lambda.chunk.push_op(OpCode::OpForce, span);
        lambda.chunk.push_op(OpCode::OpCall, span);

        // Inform the VM that the chunk has ended
        lambda.chunk.push_op(OpCode::OpReturn, span);

        Thunk(Rc::new(RefCell::new(ThunkRepr::Suspended {
            upvalues: Rc::new(Upvalues::with_capacity(0)),
            lambda: Rc::new(lambda),
            light_span,
        })))
    }

    fn prepare_blackhole(
        &self,
        forced_at: LightSpan,
        prev_blackhole: Option<Rc<RefCell<ThunkRepr>>>,
    ) -> ThunkRepr {
        match &*self.0.borrow() {
            ThunkRepr::Suspended {
                light_span, lambda, ..
            } => ThunkRepr::Blackhole {
                forced_at,
                suspended_at: Some(light_span.clone()),
                content_span: Some(lambda.chunk.first_span()),
                prev_ptr: prev_blackhole,
            },

            _ => ThunkRepr::Blackhole {
                forced_at,
                suspended_at: None,
                content_span: None,
                prev_ptr: prev_blackhole,
            },
        }
    }

    pub async fn force(myself: Thunk, co: GenCo, span: LightSpan) -> Result<Value, ErrorKind> {
        Self::force_(myself, &co, span).await
    }
    pub async fn force_(
        mut myself: Thunk,
        co: &GenCo,
        span: LightSpan,
    ) -> Result<Value, ErrorKind> {
        // It is possible to have a long chain of thunks, like
        //
        //   ThunkRepr::Evaluated(Value::Thunk(ThunkRepr::Evaluated(...)))
        //
        // When forcing a long chain of thunks like this we will
        // walk down to the end of the chain, force the final thunk,
        // and update it.  However if we don't update all the thunks
        // along the chain, two problems occur:
        //
        // 1. The amount of work needed to force the first thunk in
        //    the chain is O(chain_length) instead of O(1)
        //
        // 2. VM code that assumes !value.force().is_thunk() will fail.
        //
        // Therefore, after forcing the thunk at the end of the
        // chain, we need to walk backwards and update *all* the
        // thunks.  We maintain a linked list out of the
        // ThunkRepr::Blackhole values, and walk that linked list
        // once we have finished forcing to a ground value.
        //
        // The value `last_blackhole` below holds the most-recent
        // blackhole.
        //
        let mut last_blackhole: Option<Rc<RefCell<ThunkRepr>>> = None;

        loop {
            // If the current thunk is already fully evaluated, return its evaluated
            // value. The VM will continue running the code that landed us here.
            if let Some(depth) = myself.get_forcing_depth() {
                let val = myself.unwrap_or_clone();
                loop {
                    match last_blackhole {
                        None => return Ok(val),
                        Some(repr) => {
                            // TODO: clone the thunkrepr, not the value
                            last_blackhole =
                                match repr.replace(ThunkRepr::Evaluated(val.clone(), depth)) {
                                    ThunkRepr::Blackhole { prev_ptr, .. } => prev_ptr,
                                    _ => unreachable!(),
                                };
                        }
                    }
                }
            }

            // Begin evaluation of this thunk by marking it as a blackhole, meaning
            // that any other forcing frame encountering this thunk before its
            // evaluation is completed detected an evaluation cycle.
            let repr = myself
                .0
                .replace(myself.prepare_blackhole(span.clone(), last_blackhole.clone()));

            match repr {
                // If there was already a blackhole in the thunk, this is an
                // evaluation cycle.
                ThunkRepr::Blackhole {
                    forced_at,
                    suspended_at,
                    content_span,
                    prev_ptr,
                } => {
                    return Err(ErrorKind::InfiniteRecursion {
                        first_force: forced_at.span(),
                        suspended_at: suspended_at.map(|s| s.span()),
                        content_span,
                    })
                }

                // If there is a native function stored in the thunk, evaluate it
                // and replace this thunk's representation with the result.
                ThunkRepr::Native(native) => {
                    let value = native.0()?;
                    myself
                        .0
                        .replace(ThunkRepr::Evaluated(value, ForcingDepth::Shallowly));
                    continue;
                }

                // When encountering a suspended thunk, request that the VM enters
                // it and produces the result.
                ThunkRepr::Suspended {
                    lambda,
                    upvalues,
                    light_span,
                } => {
                    // TODO(amjoseph): use #[tailcall::mutual] here.  This can
                    // be turned into a tailcall to vm::execute_bytecode() by
                    // passing `head_of_chain_of_thunks` to it.
                    let value =
                        generators::request_enter_lambda(co, lambda, upvalues, light_span).await;
                    myself
                        .0
                        .replace(ThunkRepr::Evaluated(value, ForcingDepth::Shallowly));
                    continue;
                }

                // nested thunks -- try to flatten before forcing
                ThunkRepr::Evaluated(Value::Thunk(inner_thunk), _) => {
                    match Rc::try_unwrap(inner_thunk.0) {
                        Ok(refcell) => {
                            // we are the only reference to the inner thunk,
                            // so steal it
                            myself.0.replace(refcell.into_inner());
                            continue;
                        }
                        Err(rc) => {
                            let inner_thunk = Thunk(rc);
                            if let Some(depth) = inner_thunk.get_forcing_depth() {
                                // The inner thunk is already forced.
                                myself.0.replace(ThunkRepr::Evaluated(
                                    inner_thunk.value().clone(),
                                    depth,
                                ));
                                continue;
                            }
                            last_blackhole = Some(myself.0.clone());
                            myself = inner_thunk;
                            continue;
                        }
                    }
                }

                ThunkRepr::Evaluated(val, _) => continue,
            }
        }
    }

    pub fn finalise(&self, stack: &[Value]) {
        self.upvalues_mut().resolve_deferred_upvalues(stack);
    }

    pub fn is_evaluated(&self) -> bool {
        matches!(*self.0.borrow(), ThunkRepr::Evaluated(_, _))
    }

    pub fn is_suspended(&self) -> bool {
        matches!(
            *self.0.borrow(),
            ThunkRepr::Suspended { .. } | ThunkRepr::Native(_)
        )
    }

    /// Returns true if forcing this thunk will not change it.
    pub fn is_forced(&self) -> bool {
        self.0.borrow().is_forced()
    }

    /// Returns a reference to the inner evaluated value of a thunk.
    /// It is an error to call this on a thunk that has not been
    /// forced, or is not otherwise known to be fully evaluated.
    // Note: Due to the interior mutability of thunks this is
    // difficult to represent in the type system without impacting the
    // API too much.
    pub fn value(&self) -> Ref<Value> {
        Ref::map(self.0.borrow(), |thunk| match thunk {
            ThunkRepr::Evaluated(value, _) => value,
            ThunkRepr::Blackhole { .. } => panic!("Thunk::value called on a black-holed thunk"),
            ThunkRepr::Suspended { .. } | ThunkRepr::Native(_) => {
                panic!("Thunk::value called on a suspended thunk")
            }
        })
    }

    /// Returns the inner evaluated value of a thunk, cloning it if
    /// the Rc has more than one strong reference.  It is an error
    /// to call this on a thunk that has not been forced, or is not
    /// otherwise known to be fully evaluated.
    fn unwrap_or_clone(self) -> Value {
        match Rc::try_unwrap(self.0) {
            Ok(refcell) => refcell.into_inner().expect(),
            Err(rc) => Ref::map(rc.borrow(), |thunkrepr| thunkrepr.expect_ref()).clone(),
        }
    }

    pub fn upvalues(&self) -> Ref<'_, Upvalues> {
        Ref::map(self.0.borrow(), |thunk| match thunk {
            ThunkRepr::Suspended { upvalues, .. } => upvalues.as_ref(),
            ThunkRepr::Evaluated(Value::Closure(c), _) => &c.upvalues,
            _ => panic!("upvalues() on non-suspended thunk"),
        })
    }

    pub fn upvalues_mut(&self) -> RefMut<'_, Upvalues> {
        RefMut::map(self.0.borrow_mut(), |thunk| match thunk {
            ThunkRepr::Suspended { upvalues, .. } => Rc::get_mut(upvalues).unwrap(),
            ThunkRepr::Evaluated(Value::Closure(c), _) => Rc::get_mut(
                &mut Rc::get_mut(c).unwrap().upvalues,
            )
            .expect(
                "upvalues_mut() was called on a thunk which already had multiple references to it",
            ),
            thunk => panic!("upvalues() on non-suspended thunk: {thunk:?}"),
        })
    }

    /// Do not use this without first reading and understanding
    /// `tvix/docs/value-pointer-equality.md`.
    pub(crate) fn ptr_eq(&self, other: &Self) -> bool {
        if Rc::ptr_eq(&self.0, &other.0) {
            return true;
        }
        match &*self.0.borrow() {
            ThunkRepr::Evaluated(Value::Closure(c1), _) => match &*other.0.borrow() {
                ThunkRepr::Evaluated(Value::Closure(c2), _) => Rc::ptr_eq(c1, c2),
                _ => false,
            },
            _ => false,
        }
    }

    /// Helper function to format thunks in observer output.
    pub(crate) fn debug_repr(&self) -> String {
        self.0.borrow().debug_repr()
    }
}

impl TotalDisplay for Thunk {
    fn total_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        set: &mut ThunkFormatter,
    ) -> std::fmt::Result {
        if !set.insert(self) {
            return f.write_str("<CYCLE>");
        }

        match &*self.0.borrow() {
            ThunkRepr::Evaluated(v, _) => v.total_fmt(f, set),
            ThunkRepr::Suspended { .. } | ThunkRepr::Native(_) => f.write_str("<CODE>"),
            other => write!(f, "internal[{}]", other.debug_repr()),
        }
    }
}

/// A wrapper type for tracking which thunks have already been seen
/// in a context. This is necessary for printing and deeply forcing
/// cyclic non-diverging data structures like `rec { f = [ f ]; }`.
/// This is separate from the ThunkRepr::Blackhole mechanism, which
/// detects diverging data structures like `(rec { f = f; }).f`.
///
/// The inner `HashSet` is not available on the outside, as it would be
/// potentially unsafe to interact with the pointers in the set.
#[derive(Default)]
pub struct ThunkFormatter(HashSet<*const ThunkRepr>);

impl ThunkFormatter {
    /// Check whether the given thunk has already been seen. Will mark the thunk
    /// as seen otherwise.
    pub fn insert(&mut self, thunk: &Thunk) -> bool {
        let ptr: *const ThunkRepr = thunk.0.as_ptr();
        self.0.insert(ptr)
    }
}
