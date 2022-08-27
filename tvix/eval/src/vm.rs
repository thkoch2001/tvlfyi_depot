//! This module implements the virtual (or abstract) machine that runs
//! Tvix bytecode.

use std::rc::Rc;

use crate::{
    chunk::Chunk,
    errors::{ErrorKind, EvalResult},
    opcode::{Count, JumpOffset, OpCode, StackIdx, UpvalueIdx},
    value::{Closure, Lambda, NixAttrs, NixList, Value},
};

#[cfg(feature = "disassembler")]
use crate::disassembler::Tracer;

struct CallFrame {
    closure: Closure,
    ip: usize,
    stack_offset: usize,
}

pub struct VM {
    frames: Vec<CallFrame>,
    stack: Vec<Value>,

    // Stack indices of attribute sets from which variables should be
    // dynamically resolved (`with`).
    with_stack: Vec<usize>,
}

macro_rules! arithmetic_op {
    ( $self:ident, $op:tt ) => {{
        let b = $self.pop();
        let a = $self.pop();
        let result = arithmetic_op!(a, b, $op);
        $self.push(result);
    }};

    ( $a:ident, $b:ident, $op:tt ) => {{
        match ($a, $b) {
            (Value::Integer(i1), Value::Integer(i2)) => Value::Integer(i1 $op i2),
            (Value::Float(f1), Value::Float(f2)) => Value::Float(f1 $op f2),
            (Value::Integer(i1), Value::Float(f2)) => Value::Float(i1 as f64 $op f2),
            (Value::Float(f1), Value::Integer(i2)) => Value::Float(f1 $op i2 as f64),

            (v1, v2) => return Err(ErrorKind::TypeError {
                expected: "number (either int or float)",
                actual: if v1.is_number() {
                    v2.type_of()
                } else {
                    v1.type_of()
                },
            }.into()),
        }
    }};
}

macro_rules! cmp_op {
    ( $self:ident, $op:tt ) => {{
        let b = $self.pop();
        let a = $self.pop();

        // Comparable (in terms of ordering) values are numbers and
        // strings. Numbers need to be coerced similarly to arithmetic
        // ops if mixed types are encountered.
        let result = match (a, b) {
            (Value::Integer(i1), Value::Integer(i2)) => i1 $op i2,
            (Value::Float(f1), Value::Float(f2)) => f1 $op f2,
            (Value::Integer(i1), Value::Float(f2)) => (i1 as f64) $op f2,
            (Value::Float(f1), Value::Integer(i2)) => f1 $op (i2 as f64),
            (Value::String(s1), Value::String(s2)) => s1 $op s2,

            (lhs, rhs) => return Err(ErrorKind::Incomparable {
                lhs: lhs.type_of(),
                rhs: rhs.type_of(),
            }.into()),
        };

        $self.push(Value::Bool(result));
    }};
}

impl VM {
    fn frame(&self) -> &CallFrame {
        &self.frames[self.frames.len() - 1]
    }

    fn chunk(&self) -> &Chunk {
        &self.frame().closure.lambda.chunk
    }

    fn frame_mut(&mut self) -> &mut CallFrame {
        let idx = self.frames.len() - 1;
        &mut self.frames[idx]
    }

    fn inc_ip(&mut self) -> OpCode {
        let op = self.chunk().code[self.frame().ip];
        self.frame_mut().ip += 1;
        op
    }

    fn pop(&mut self) -> Value {
        self.stack.pop().expect("runtime stack empty")
    }

    fn push(&mut self, value: Value) {
        self.stack.push(value)
    }

    fn peek(&self, offset: usize) -> &Value {
        &self.stack[self.stack.len() - 1 - offset]
    }

    fn call(&mut self, closure: Closure, arg_count: usize) {
        let frame = CallFrame {
            closure,
            ip: 0,
            stack_offset: self.stack.len() - arg_count,
        };

        self.frames.push(frame);
    }

    fn run(&mut self) -> EvalResult<Value> {
        #[cfg(feature = "disassembler")]
        let mut tracer = Tracer::new();

        loop {
            if self.frame().ip == self.chunk().code.len() {
                // If this is the end of the top-level function,
                // return, otherwise pop the call frame.
                if self.frames.len() == 1 {
                    return Ok(self.pop());
                }

                self.frames.pop();
                continue;
            }

            let op = self.inc_ip();
            match op {
                OpCode::OpConstant(idx) => {
                    let c = self.chunk().constant(idx).clone();
                    self.push(c);
                }

                OpCode::OpPop => {
                    self.pop();
                }

                OpCode::OpAdd => {
                    let b = self.pop();
                    let a = self.pop();

                    let result = if let (Value::String(s1), Value::String(s2)) = (&a, &b) {
                        Value::String(s1.concat(s2))
                    } else {
                        arithmetic_op!(a, b, +)
                    };

                    self.push(result)
                }

                OpCode::OpSub => arithmetic_op!(self, -),
                OpCode::OpMul => arithmetic_op!(self, *),
                OpCode::OpDiv => arithmetic_op!(self, /),

                OpCode::OpInvert => {
                    let v = self.pop().as_bool()?;
                    self.push(Value::Bool(!v));
                }

                OpCode::OpNegate => match self.pop() {
                    Value::Integer(i) => self.push(Value::Integer(-i)),
                    Value::Float(f) => self.push(Value::Float(-f)),
                    v => {
                        return Err(ErrorKind::TypeError {
                            expected: "number (either int or float)",
                            actual: v.type_of(),
                        }
                        .into())
                    }
                },

                OpCode::OpEqual => {
                    let v2 = self.pop();
                    let v1 = self.pop();

                    self.push(Value::Bool(v1 == v2))
                }

                OpCode::OpLess => cmp_op!(self, <),
                OpCode::OpLessOrEq => cmp_op!(self, <=),
                OpCode::OpMore => cmp_op!(self, >),
                OpCode::OpMoreOrEq => cmp_op!(self, >=),

                OpCode::OpNull => self.push(Value::Null),
                OpCode::OpTrue => self.push(Value::Bool(true)),
                OpCode::OpFalse => self.push(Value::Bool(false)),

                OpCode::OpAttrs(Count(count)) => self.run_attrset(count)?,
                OpCode::OpAttrPath(Count(count)) => self.run_attr_path(count)?,

                OpCode::OpAttrsUpdate => {
                    let rhs = unwrap_or_clone_rc(self.pop().to_attrs()?);
                    let lhs = unwrap_or_clone_rc(self.pop().to_attrs()?);

                    self.push(Value::Attrs(Rc::new(lhs.update(rhs))))
                }

                OpCode::OpAttrsSelect => {
                    let key = self.pop().to_string()?;
                    let attrs = self.pop().to_attrs()?;

                    match attrs.select(key.as_str()) {
                        Some(value) => self.push(value.clone()),

                        None => {
                            return Err(ErrorKind::AttributeNotFound {
                                name: key.as_str().to_string(),
                            }
                            .into())
                        }
                    }
                }

                OpCode::OpAttrsTrySelect => {
                    let key = self.pop().to_string()?;
                    let value = match self.pop() {
                        Value::Attrs(attrs) => match attrs.select(key.as_str()) {
                            Some(value) => value.clone(),
                            None => Value::AttrNotFound,
                        },

                        _ => Value::AttrNotFound,
                    };

                    self.push(value);
                }

                OpCode::OpAttrsIsSet => {
                    let key = self.pop().to_string()?;
                    let result = match self.pop() {
                        Value::Attrs(attrs) => attrs.contains(key.as_str()),

                        // Nix allows use of `?` on non-set types, but
                        // always returns false in those cases.
                        _ => false,
                    };

                    self.push(Value::Bool(result));
                }

                OpCode::OpList(Count(count)) => {
                    let list =
                        NixList::construct(count, self.stack.split_off(self.stack.len() - count));
                    self.push(Value::List(list));
                }

                OpCode::OpConcat => {
                    let rhs = self.pop().to_list()?;
                    let lhs = self.pop().to_list()?;
                    self.push(Value::List(lhs.concat(&rhs)))
                }

                OpCode::OpInterpolate(Count(count)) => self.run_interpolate(count)?,

                OpCode::OpJump(JumpOffset(offset)) => {
                    self.frame_mut().ip += offset;
                }

                OpCode::OpJumpIfTrue(JumpOffset(offset)) => {
                    if self.peek(0).as_bool()? {
                        self.frame_mut().ip += offset;
                    }
                }

                OpCode::OpJumpIfFalse(JumpOffset(offset)) => {
                    if !self.peek(0).as_bool()? {
                        self.frame_mut().ip += offset;
                    }
                }

                OpCode::OpJumpIfNotFound(JumpOffset(offset)) => {
                    if matches!(self.peek(0), Value::AttrNotFound) {
                        self.pop();
                        self.frame_mut().ip += offset;
                    }
                }

                // These assertion operations error out if the stack
                // top is not of the expected type. This is necessary
                // to implement some specific behaviours of Nix
                // exactly.
                OpCode::OpAssertBool => {
                    let val = self.peek(0);
                    if !val.is_bool() {
                        return Err(ErrorKind::TypeError {
                            expected: "bool",
                            actual: val.type_of(),
                        }
                        .into());
                    }
                }

                // Remove the given number of elements from the stack,
                // but retain the top value.
                OpCode::OpCloseScope(Count(count)) => {
                    // Immediately move the top value into the right
                    // position.
                    let target_idx = self.stack.len() - 1 - count;
                    self.stack[target_idx] = self.pop();

                    // Then drop the remaining values.
                    for _ in 0..(count - 1) {
                        self.pop();
                    }
                }

                OpCode::OpGetLocal(StackIdx(local_idx)) => {
                    let idx = self.frame().stack_offset + local_idx;
                    self.push(self.stack[idx].clone());
                }

                OpCode::OpPushWith(StackIdx(idx)) => self.with_stack.push(idx),
                OpCode::OpPopWith => {
                    self.with_stack.pop();
                }

                OpCode::OpResolveWith => {
                    let ident = self.pop().to_string()?;
                    let value = self.resolve_with(ident.as_str())?;
                    self.push(value)
                }

                OpCode::OpAssert => {
                    if !self.pop().as_bool()? {
                        return Err(ErrorKind::AssertionFailed.into());
                    }
                }

                OpCode::OpCall => {
                    let callable = self.pop();
                    match callable {
                        Value::Closure(closure) => self.call(closure, 1),
                        Value::Builtin(builtin) => {
                            let arg = self.pop();
                            let result = builtin.apply(arg)?;
                            self.push(result);
                        }
                        _ => return Err(ErrorKind::NotCallable.into()),
                    };
                }

                OpCode::OpGetUpvalue(UpvalueIdx(upv_idx)) => {
                    let value = self.frame().closure.upvalues[upv_idx].clone();
                    self.push(value);
                }

                OpCode::OpClosure(idx) => {
                    let mut closure = self.chunk().constant(idx).clone().to_closure()?;

                    debug_assert!(
                        closure.lambda.upvalue_count > 0,
                        "OpClosure should not be called for plain lambdas"
                    );

                    for _ in 0..closure.lambda.upvalue_count {
                        match self.inc_ip() {
                            OpCode::DataLocalIdx(StackIdx(local_idx)) => {
                                let idx = self.frame().stack_offset + local_idx;
                                closure.upvalues.push(self.stack[idx].clone());
                            }

                            OpCode::DataUpvalueIdx(UpvalueIdx(upv_idx)) => {
                                closure
                                    .upvalues
                                    .push(self.frame().closure.upvalues[upv_idx].clone());
                            }

                            OpCode::DataDynamicIdx(ident_idx) => {
                                let ident = self.chunk().constant(ident_idx).as_str()?;
                                closure.upvalues.push(self.resolve_with(ident)?);
                            }

                            _ => panic!("compiler error: missing closure operand"),
                        }
                    }

                    self.push(Value::Closure(closure));
                }

                // Data-carrying operands should never be executed,
                // that is a critical error in the VM.
                OpCode::DataLocalIdx(_) | OpCode::DataUpvalueIdx(_) | OpCode::DataDynamicIdx(_) => {
                    panic!("VM bug: attempted to execute data-carrying operand")
                }
            }

            #[cfg(feature = "disassembler")]
            {
                tracer.trace(&op, self.frame().ip, &self.stack);
            }
        }
    }

    // Construct runtime representation of an attr path (essentially
    // just a list of strings).
    //
    // The difference to the list construction operation is that this
    // forces all elements into strings, as attribute set keys are
    // required to be strict in Nix.
    fn run_attr_path(&mut self, count: usize) -> EvalResult<()> {
        debug_assert!(count > 1, "AttrPath needs at least two fragments");
        let mut path = Vec::with_capacity(count);

        for _ in 0..count {
            path.push(self.pop().to_string()?);
        }

        self.push(Value::AttrPath(path));
        Ok(())
    }

    fn run_attrset(&mut self, count: usize) -> EvalResult<()> {
        let attrs = NixAttrs::construct(count, self.stack.split_off(self.stack.len() - count * 2))?;
        self.push(Value::Attrs(Rc::new(attrs)));
        Ok(())
    }

    // Interpolate string fragments by popping the specified number of
    // fragments of the stack, evaluating them to strings, and pushing
    // the concatenated result string back on the stack.
    fn run_interpolate(&mut self, count: usize) -> EvalResult<()> {
        let mut out = String::new();

        for _ in 0..count {
            out.push_str(self.pop().to_string()?.as_str());
        }

        self.push(Value::String(out.into()));
        Ok(())
    }

    /// Resolve a dynamic identifier through the with-stack at runtime.
    fn resolve_with(&self, ident: &str) -> EvalResult<Value> {
        for idx in self.with_stack.iter().rev() {
            let with = self.stack[*idx].as_attrs()?;
            match with.select(ident) {
                None => continue,
                Some(val) => return Ok(val.clone()),
            }
        }

        Err(ErrorKind::UnknownDynamicVariable(ident.to_string()).into())
    }
}

// TODO: use Rc::unwrap_or_clone once it is stabilised.
// https://doc.rust-lang.org/std/rc/struct.Rc.html#method.unwrap_or_clone
fn unwrap_or_clone_rc<T: Clone>(rc: Rc<T>) -> T {
    Rc::try_unwrap(rc).unwrap_or_else(|rc| (*rc).clone())
}

pub fn run_lambda(lambda: Lambda) -> EvalResult<Value> {
    let mut vm = VM {
        frames: vec![],
        stack: vec![],
        with_stack: vec![],
    };

    vm.call(Closure::new(lambda), 0);
    vm.run()
}
