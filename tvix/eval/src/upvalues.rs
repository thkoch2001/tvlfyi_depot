//! This module encapsulates some logic for upvalue handling, which is
//! relevant to both thunks (delayed computations for lazy-evaluation)
//! as well as closures (lambdas that capture variables from the
//! surrounding scope).

use std::ops::Index;

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

    /// Resolve deferred upvalues from the provided stack slice,
    /// mutating them in the internal upvalue slots.
    pub fn resolve_deferred_upvalues(&mut self, stack: &[Value]) {
        for upvalue in self.upvalues.iter_mut() {
            if let Value::DeferredUpvalue(update_from_idx) = upvalue {
                *upvalue = stack[update_from_idx.0].clone();
            }
        }
    }
}

impl Index<UpvalueIdx> for Upvalues {
    type Output = Value;

    fn index(&self, index: UpvalueIdx) -> &Self::Output {
        &self.upvalues[index.0]
    }
}
