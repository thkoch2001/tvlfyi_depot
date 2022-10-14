//! This module encapsulates some logic for upvalue handling, which is
//! relevant to both thunks (delayed computations for lazy-evaluation)
//! as well as closures (lambdas that capture variables from the
//! surrounding scope).

use std::{
    cell::{Ref, RefCell, RefMut},
    ops::Index,
    rc::Rc,
};

use crate::{opcode::UpvalueIdx, Value};

/// Structure for carrying upvalues inside of thunks & closures. The
/// implementation of this struct encapsulates the logic for capturing
/// and accessing upvalues.
#[derive(Clone, Debug, PartialEq)]
pub struct Upvalues {
    upvalues: Vec<Value>,
    with_stack: Option<Vec<Value>>,
}

impl Upvalues {
    pub fn with_capacity(count: usize) -> Self {
        Upvalues {
            upvalues: Vec::with_capacity(count),
            with_stack: None,
        }
    }

    /// Push an upvalue at the end of the upvalue list.
    pub fn push(&mut self, value: Value) {
        self.upvalues.push(value);
    }

    /// Set the captured with stack.
    pub fn set_with_stack(&mut self, with_stack: Vec<Value>) {
        self.with_stack = Some(with_stack);
    }

    pub fn with_stack(&self) -> Option<&Vec<Value>> {
        self.with_stack.as_ref()
    }

    pub fn with_stack_len(&self) -> usize {
        match &self.with_stack {
            None => 0,
            Some(stack) => stack.len(),
        }
    }
}

impl Index<UpvalueIdx> for Upvalues {
    type Output = Value;

    fn index(&self, index: UpvalueIdx) -> &Self::Output {
        &self.upvalues[index.0]
    }
}

/// `UpvalueCarrier` is implemented by all types that carry upvalues.
pub trait UpvalueCarrier<'a, U>
where
    U: AsMut<Upvalues> + AsRef<crate::value::InnerClosure> + 'a,
{
    fn upvalue_count(&self) -> usize;

    /// Read-only accessor for the stored upvalues.
    fn upvalues(&self) -> Ref<'_, U>;

    /// Mutable accessor for stored upvalues.
    fn upvalues_mut(&self) -> RefMut<'_, U>;

    /// Read an upvalue at the given index.
    fn upvalue(&'a self, idx: UpvalueIdx) -> Ref<'a, Value> {
        Ref::map(self.upvalues(), |v| &v.as_ref().upvalues.upvalues[idx.0])
    }

    /// Resolve deferred upvalues from the provided stack slice,
    /// mutating them in the internal upvalue slots.
    fn resolve_deferred_upvalues(&'a self, stack: &[Value]) {
        RefMut::map(self.upvalues_mut(), |upvalues| {
            for upvalue in upvalues.as_mut().upvalues.iter_mut() {
                if let Value::DeferredUpvalue(idx) = upvalue {
                    *upvalue = stack[idx.0].clone();
                }
            }
            upvalues
        });
    }
}

impl<'a, T, U> UpvalueCarrier<'a, U> for T
where
    T: AsRef<Rc<RefCell<U>>>,
    U: AsMut<Upvalues> + AsRef<crate::value::InnerClosure> + 'a,
{
    fn upvalue_count(&self) -> usize {
        self.as_ref().borrow().as_ref().lambda.upvalue_count
    }

    fn upvalues(&self) -> Ref<'_, U> {
        self.as_ref().borrow()
    }

    fn upvalues_mut(&self) -> RefMut<'_, U> {
        self.as_ref().borrow_mut()
    }
}
