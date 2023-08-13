//! This module encapsulates some logic for upvalue handling, which is
//! relevant to both thunks (delayed computations for lazy-evaluation)
//! as well as closures (lambdas that capture variables from the
//! surrounding scope).
//!
//! The upvalues of a scope are whatever data are needed at runtime
//! in order to resolve each free variable in the scope to a value.
//! "Upvalue" is a term taken from Lua.

use gc::{Finalize, Gc, GcCell, GcCellRef, Trace};

use crate::{opcode::UpvalueIdx, Value};

/// Structure for carrying upvalues of an UpvalueCarrier.  The
/// implementation of this struct encapsulates the logic for
/// capturing and accessing upvalues.
///
/// Nix's `with` cannot be used to shadow an enclosing binding --
/// like Rust's `use xyz::*` construct, but unlike Javascript's
/// `with (xyz)`.  This means that Nix has two kinds of identifiers,
/// which can be distinguished at compile time:
///
/// - Static identifiers, which are bound in some enclosing scope by
///   `let`, `name:` or `{name}:`
/// - Dynamic identifiers, which are not bound in any enclosing
///   scope
#[derive(Clone, Debug, Finalize, Trace)]
struct UpvaluesRepr {
    /// The upvalues of static identifiers.  Each static identifier
    /// is assigned an integer identifier at compile time, which is
    /// an index into this Vec.
    static_upvalues: Vec<Value>,

    /// The upvalues of dynamic identifiers, if any exist.  This
    /// consists of the value passed to each enclosing `with val;`,
    /// from outermost to innermost.
    with_stack: Option<Vec<Value>>,
}

/// Public wrapper struct for `UpvaluesRepr` which hides the internal
/// mutability details from the user.
#[derive(Clone, Debug, Finalize, Trace)]
pub struct Upvalues(Gc<GcCell<UpvaluesRepr>>);

impl Upvalues {
    pub fn with_capacity(count: usize) -> Self {
        Upvalues(Gc::new(GcCell::new(UpvaluesRepr {
            static_upvalues: Vec::with_capacity(count),
            with_stack: None,
        })))
    }

    /// Push an upvalue at the end of the upvalue list.
    pub fn push(&self, value: Value) {
        self.0.borrow_mut().static_upvalues.push(value);
    }

    /// Set the captured with stack.
    pub fn set_with_stack(&self, with_stack: Vec<Value>) {
        self.0.borrow_mut().with_stack = Some(with_stack);
    }

    /// Returns a copy of the current with stack. This returns an
    /// owned value because the only caller needs an owned value
    /// anyways, and returning a reference here would be needlessly
    /// complex.
    pub fn with_stack(&self) -> Option<Vec<Value>> {
        self.0.borrow().with_stack.clone()
    }

    pub fn with_stack_len(&self) -> usize {
        match &self.0.borrow().with_stack {
            None => 0,
            Some(stack) => stack.len(),
        }
    }

    /// Resolve deferred upvalues from the provided stack slice,
    /// mutating them in the internal upvalue slots.
    pub fn resolve_deferred_upvalues(&self, stack: &[Value]) {
        for upvalue in self.0.borrow_mut().static_upvalues.iter_mut() {
            if let Value::DeferredUpvalue(update_from_idx) = upvalue {
                *upvalue = stack[update_from_idx.0].clone();
            }
        }
    }

    /// Get the upvalue at the given index, or panic otherwise. This
    /// is similar to the `Index` trait, which we can not use due to
    /// the internal mutability.
    pub fn get(&self, index: UpvalueIdx) -> GcCellRef<Value> {
        GcCellRef::map(self.0.borrow(), |u| &u.static_upvalues[index.0])
    }
}
