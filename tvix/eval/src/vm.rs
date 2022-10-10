//! This module implements the virtual (or abstract) machine that runs
//! Tvix bytecode.

use std::{cell::RefMut, path::PathBuf, rc::Rc};

use path_clean::PathClean;

use crate::{
    chunk::Chunk,
    errors::{Error, ErrorKind, EvalResult},
    nix_search_path::NixSearchPath,
    observer::RuntimeObserver,
    opcode::{CodeIdx, Count, JumpOffset, OpCode, StackIdx, UpvalueIdx},
    upvalues::{UpvalueCarrier, Upvalues},
    value::{Builtin, Closure, CoercionKind, Lambda, NixAttrs, NixList, Thunk, Value},
    warnings::{EvalWarning, WarningKind},
};

struct CallFrame {
    /// The lambda currently being executed.
    lambda: Rc<Lambda>,

    /// Optional captured upvalues of this frame (if a thunk or
    /// closure if being evaluated).
    upvalues: Upvalues,

    /// Instruction pointer to the instruction currently being
    /// executed.
    ip: CodeIdx,

    /// Stack offset, i.e. the frames "view" into the VM's full stack.
    stack_offset: usize,
}

impl CallFrame {
    /// Retrieve an upvalue from this frame at the given index.
    fn upvalue(&self, idx: UpvalueIdx) -> &Value {
        &self.upvalues[idx]
    }
}

pub struct VM<'o> {
    frames: Vec<CallFrame>,
    stack: Vec<Value>,

    /// Stack indices of attribute sets from which variables should be
    /// dynamically resolved (`with`).
    with_stack: Vec<usize>,

    /// Runtime warnings collected during evaluation.
    warnings: Vec<EvalWarning>,

    nix_search_path: NixSearchPath,

    observer: &'o mut dyn RuntimeObserver,
}

/// The result of a VM's runtime evaluation.
pub struct RuntimeResult {
    pub value: Value,
    pub warnings: Vec<EvalWarning>,
}

/// This macro wraps a computation that returns an ErrorKind or a
/// result, and wraps the ErrorKind in an Error struct if present.
///
/// The reason for this macro's existence is that calculating spans is
/// potentially expensive, so it should be avoided to the last moment
/// (i.e. definite instantiation of a runtime error) if possible.
macro_rules! fallible {
    ( $self:ident, $body:expr) => {
        match $body {
            Ok(result) => result,
            Err(kind) => {
                return Err(Error {
                    kind,
                    span: $self.current_span(),
                })
            }
        }
    };
}

#[macro_export]
macro_rules! arithmetic_op {
    ( $self:ident, $op:tt ) => {{
        let b = $self.pop();
        let a = $self.pop();
        let result = fallible!($self, arithmetic_op!(&a, &b, $op));
        $self.push(result);
    }};

    ( $a:expr, $b:expr, $op:tt ) => {{
        match ($a, $b) {
            (Value::Integer(i1), Value::Integer(i2)) => Ok(Value::Integer(i1 $op i2)),
            (Value::Float(f1), Value::Float(f2)) => Ok(Value::Float(f1 $op f2)),
            (Value::Integer(i1), Value::Float(f2)) => Ok(Value::Float(*i1 as f64 $op f2)),
            (Value::Float(f1), Value::Integer(i2)) => Ok(Value::Float(f1 $op *i2 as f64)),

            (v1, v2) => Err(ErrorKind::TypeError {
                expected: "number (either int or float)",
                actual: if v1.is_number() {
                    v2.type_of()
                } else {
                    v1.type_of()
                },
            }),
        }
    }};
}

#[macro_export]
macro_rules! cmp_op {
    ( $self:ident, $op:tt ) => {{
        let b = $self.pop();
        let a = $self.pop();
        let result = fallible!($self, cmp_op!(&a, &b, $op));
        $self.push(result);
    }};

    ( $a:expr, $b:expr, $op:tt ) => {
        // Comparable (in terms of ordering) values are numbers and
        // strings. Numbers need to be coerced similarly to arithmetic
        // ops if mixed types are encountered.
        match ($a, $b) {
            // same types
            (Value::Integer(i1), Value::Integer(i2)) => Ok(Value::Bool(i1 $op i2)),
            (Value::Float(f1), Value::Float(f2)) => Ok(Value::Bool(f1 $op f2)),
            (Value::String(s1), Value::String(s2)) => Ok(Value::Bool(s1 $op s2)),

            // different types
            (Value::Integer(i1), Value::Float(f2)) => Ok(Value::Bool((*i1 as f64) $op *f2)),
            (Value::Float(f1), Value::Integer(i2)) => Ok(Value::Bool(*f1 $op (*i2 as f64))),

            // unsupported types
            (lhs, rhs) => Err(ErrorKind::Incomparable {
                lhs: lhs.type_of(),
                rhs: rhs.type_of(),
            }),
        }
    }
}

impl<'o> VM<'o> {
    pub fn new(nix_search_path: NixSearchPath, observer: &'o mut dyn RuntimeObserver) -> Self {
        Self {
            nix_search_path,
            observer,
            frames: vec![],
            stack: vec![],
            with_stack: vec![],
            warnings: vec![],
        }
    }

    fn frame(&self) -> &CallFrame {
        &self.frames[self.frames.len() - 1]
    }

    fn chunk(&self) -> &Chunk {
        &self.frame().lambda.chunk
    }

    fn frame_mut(&mut self) -> &mut CallFrame {
        let idx = self.frames.len() - 1;
        &mut self.frames[idx]
    }

    fn inc_ip(&mut self) -> OpCode {
        let op = self.chunk()[self.frame().ip];
        self.frame_mut().ip += 1;
        op
    }

    pub fn pop(&mut self) -> Value {
        self.stack.pop().expect("runtime stack empty")
    }

    pub fn push(&mut self, value: Value) {
        self.stack.push(value)
    }

    fn peek(&self, offset: usize) -> &Value {
        &self.stack[self.stack.len() - 1 - offset]
    }

    /// Returns the source span of the instruction currently being
    /// executed.
    pub(crate) fn current_span(&self) -> codemap::Span {
        self.chunk().get_span(self.frame().ip - 1)
    }

    /// Construct an error from the given ErrorKind and the source
    /// span of the current instruction.
    pub fn error(&self, kind: ErrorKind) -> Error {
        Error {
            kind,
            span: self.current_span(),
        }
    }

    /// Push an already constructed warning.
    pub fn push_warning(&mut self, warning: EvalWarning) {
        self.warnings.push(warning);
    }

    /// Emit a warning with the given WarningKind and the source span
    /// of the current instruction.
    pub fn emit_warning(&mut self, kind: WarningKind) {
        self.push_warning(EvalWarning {
            kind,
            span: self.current_span(),
        });
    }

    /// Execute the given value in this VM's context, if it is a
    /// callable.
    ///
    /// The stack of the VM must be prepared with all required
    /// arguments before calling this and the value must have already
    /// been forced.
    pub fn call_value(&mut self, callable: &Value) -> EvalResult<()> {
        match callable {
            Value::Closure(c) => self.enter_frame(c.lambda(), c.upvalues().clone(), 1),

            Value::Builtin(b) => self.call_builtin(b.clone()),

            Value::Thunk(t) => self.call_value(&t.value()),

            // Attribute sets with a __functor attribute are callable.
            Value::Attrs(ref attrs) => match attrs.select("__functor") {
                None => Err(self.error(ErrorKind::NotCallable(callable.type_of()))),
                Some(functor) => {
                    // The functor receives the set itself as its first argument
                    // and needs to be called with it. However, this call is
                    // synthetic (i.e. there is no corresponding OpCall for the
                    // first call in the bytecode.)
                    self.push(callable.clone());
                    self.call_value(functor)?;
                    let primed = self.pop();
                    self.call_value(&primed)
                }
            },

            // TODO: this isn't guaranteed to be a useful span, actually
            other => Err(self.error(ErrorKind::NotCallable(other.type_of()))),
        }
    }

    /// Call the given `callable` value with the given list of `args`
    ///
    /// # Panics
    ///
    /// Panics if the passed list of `args` is empty
    #[track_caller]
    pub fn call_with<I>(&mut self, callable: &Value, args: I) -> EvalResult<Value>
    where
        I: IntoIterator<Item = Value>,
    {
        let mut num_args = 0_usize;
        for arg in args {
            num_args += 1;
            self.push(arg);
        }

        if num_args == 0 {
            panic!("call_with called with an empty list of args");
        }

        self.call_value(callable)?;
        let mut res = self.pop();

        for _ in 0..(num_args - 1) {
            self.call_value(&res)?;
            res = self.pop();
        }

        Ok(res)
    }

    fn tail_call_value(&mut self, callable: Value) -> EvalResult<()> {
        match callable {
            Value::Builtin(builtin) => self.call_builtin(builtin),
            Value::Thunk(thunk) => self.tail_call_value(thunk.value().clone()),

            Value::Closure(closure) => {
                let lambda = closure.lambda();
                self.observer.observe_tail_call(self.frames.len(), &lambda);

                // Replace the current call frames internals with
                // that of the tail-called closure.
                let mut frame = self.frame_mut();
                frame.lambda = lambda;
                frame.upvalues = closure.upvalues().clone();
                frame.ip = CodeIdx(0); // reset instruction pointer to beginning
                Ok(())
            }

            // Attribute sets with a __functor attribute are callable.
            Value::Attrs(ref attrs) => match attrs.select("__functor") {
                None => Err(self.error(ErrorKind::NotCallable(callable.type_of()))),
                Some(functor) => {
                    // The functor receives the set itself as its first argument
                    // and needs to be called with it. However, this call is
                    // synthetic (i.e. there is no corresponding OpCall for the
                    // first call in the bytecode.)
                    self.push(callable.clone());
                    self.call_value(functor)?;
                    let primed = self.pop();
                    self.tail_call_value(primed)
                }
            },

            _ => Err(self.error(ErrorKind::NotCallable(callable.type_of()))),
        }
    }

    /// Execute the given lambda in this VM's context, returning its
    /// value after its stack frame completes.
    pub fn enter_frame(
        &mut self,
        lambda: Rc<Lambda>,
        upvalues: Upvalues,
        arg_count: usize,
    ) -> EvalResult<()> {
        self.observer
            .observe_enter_frame(arg_count, &lambda, self.frames.len() + 1);

        let frame = CallFrame {
            lambda,
            upvalues,
            ip: CodeIdx(0),
            stack_offset: self.stack.len() - arg_count,
        };

        self.frames.push(frame);
        let result = self.run();

        self.observer
            .observe_exit_frame(self.frames.len() + 1, &self.stack);

        result
    }

    /// Run the VM's current call frame to completion.
    ///
    /// On successful return, the top of the stack is the value that
    /// the frame evaluated to. The frame itself is popped off. It is
    /// up to the caller to consume the value.
    fn run(&mut self) -> EvalResult<()> {
        loop {
            // Break the loop if this call frame has already run to
            // completion, pop it off, and return the value to the
            // caller.
            if self.frame().ip.0 == self.chunk().code.len() {
                self.frames.pop();
                return Ok(());
            }

            let op = self.inc_ip();

            self.observer
                .observe_execute_op(self.frame().ip, &op, &self.stack);

            let res = self.run_op(op);

            if self.frame().ip.0 == self.chunk().code.len() {
                self.frames.pop();
                return res;
            } else {
                res?;
            }
        }
    }

    fn run_op(&mut self, op: OpCode) -> EvalResult<()> {
        match op {
            OpCode::OpConstant(idx) => {
                let c = self.chunk()[idx].clone();
                self.push(c);
            }

            OpCode::OpPop => {
                self.pop();
            }

            OpCode::OpAdd => {
                let b = self.pop();
                let a = self.pop();

                let result = match (&a, &b) {
                    (Value::String(s1), Value::String(s2)) => Value::String(s1.concat(s2)),
                    (Value::Path(p), v) => {
                        let mut path = p.to_string_lossy().into_owned();
                        path.push_str(
                            &v.coerce_to_string(CoercionKind::Weak, self)
                                .map_err(|ek| self.error(ek))?,
                        );
                        PathBuf::from(path).clean().into()
                    }
                    _ => fallible!(self, arithmetic_op!(&a, &b, +)),
                };

                self.push(result)
            }

            OpCode::OpSub => arithmetic_op!(self, -),
            OpCode::OpMul => arithmetic_op!(self, *),
            OpCode::OpDiv => arithmetic_op!(self, /),

            OpCode::OpInvert => {
                let v = fallible!(self, self.pop().as_bool());
                self.push(Value::Bool(!v));
            }

            OpCode::OpNegate => match self.pop() {
                Value::Integer(i) => self.push(Value::Integer(-i)),
                Value::Float(f) => self.push(Value::Float(-f)),
                v => {
                    return Err(self.error(ErrorKind::TypeError {
                        expected: "number (either int or float)",
                        actual: v.type_of(),
                    }));
                }
            },

            OpCode::OpEqual => {
                let v2 = self.pop();
                let v1 = self.pop();
                let res = fallible!(self, v1.nix_eq(&v2, self));

                self.push(Value::Bool(res))
            }

            OpCode::OpLess => cmp_op!(self, <),
            OpCode::OpLessOrEq => cmp_op!(self, <=),
            OpCode::OpMore => cmp_op!(self, >),
            OpCode::OpMoreOrEq => cmp_op!(self, >=),

            OpCode::OpNull => self.push(Value::Null),
            OpCode::OpTrue => self.push(Value::Bool(true)),
            OpCode::OpFalse => self.push(Value::Bool(false)),

            OpCode::OpAttrs(Count(count)) => self.run_attrset(count)?,

            OpCode::OpAttrsUpdate => {
                let rhs = unwrap_or_clone_rc(fallible!(self, self.pop().to_attrs()));
                let lhs = unwrap_or_clone_rc(fallible!(self, self.pop().to_attrs()));

                self.push(Value::attrs(lhs.update(rhs)))
            }

            OpCode::OpAttrsSelect => {
                let key = fallible!(self, self.pop().to_str());
                let attrs = fallible!(self, self.pop().to_attrs());

                match attrs.select(key.as_str()) {
                    Some(value) => self.push(value.clone()),

                    None => {
                        return Err(self.error(ErrorKind::AttributeNotFound {
                            name: key.as_str().to_string(),
                        }))
                    }
                }
            }

            OpCode::OpAttrsTrySelect => {
                let key = fallible!(self, self.pop().to_str());
                let value = match self.pop() {
                    Value::Attrs(attrs) => match attrs.select(key.as_str()) {
                        Some(value) => value.clone(),
                        None => Value::AttrNotFound,
                    },

                    _ => Value::AttrNotFound,
                };

                self.push(value);
            }

            OpCode::OpHasAttr => {
                let key = fallible!(self, self.pop().to_str());
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
                let rhs = fallible!(self, self.pop().to_list());
                let lhs = fallible!(self, self.pop().to_list());
                self.push(Value::List(lhs.concat(&rhs)))
            }

            OpCode::OpInterpolate(Count(count)) => self.run_interpolate(count)?,

            OpCode::OpCoerceToString => {
                // TODO: handle string context, copying to store
                let string = fallible!(
                    self,
                    // note that coerce_to_string also forces
                    self.pop().coerce_to_string(CoercionKind::Weak, self)
                );
                self.push(Value::String(string));
            }

            OpCode::OpFindFile => {
                let path = self.pop().to_str().map_err(|e| self.error(e))?;
                let resolved = self.nix_search_path.resolve(path).map_err(|e| self.error(e))?;
                self.push(resolved.into());
            }

            OpCode::OpJump(JumpOffset(offset)) => {
                debug_assert!(offset != 0);
                self.frame_mut().ip += offset;
            }

            OpCode::OpJumpIfTrue(JumpOffset(offset)) => {
                debug_assert!(offset != 0);
                if fallible!(self, self.peek(0).as_bool()) {
                    self.frame_mut().ip += offset;
                }
            }

            OpCode::OpJumpIfFalse(JumpOffset(offset)) => {
                debug_assert!(offset != 0);
                if !fallible!(self, self.peek(0).as_bool()) {
                    self.frame_mut().ip += offset;
                }
            }

            OpCode::OpJumpIfNotFound(JumpOffset(offset)) => {
                debug_assert!(offset != 0);
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
                    return Err(self.error(ErrorKind::TypeError {
                        expected: "bool",
                        actual: val.type_of(),
                    }));
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

            OpCode::OpPushWith(StackIdx(idx)) => {
                self.with_stack.push(self.frame().stack_offset + idx)
            }

            OpCode::OpPopWith => {
                self.with_stack.pop();
            }

            OpCode::OpResolveWith => {
                let ident = fallible!(self, self.pop().to_str());
                let value = self.resolve_with(ident.as_str())?;
                self.push(value)
            }

            OpCode::OpResolveWithOrUpvalue(idx) => {
                let ident = fallible!(self, self.pop().to_str());
                match self.resolve_with(ident.as_str()) {
                    // Variable found in local `with`-stack.
                    Ok(value) => self.push(value),

                    // Variable not found => check upvalues.
                    Err(Error {
                        kind: ErrorKind::UnknownDynamicVariable(_),
                        ..
                    }) => {
                        let value = self.frame().upvalue(idx).clone();
                        self.push(value);
                    }

                    Err(err) => return Err(err),
                }
            }

            OpCode::OpAssertFail => {
                return Err(self.error(ErrorKind::AssertionFailed));
            }

            OpCode::OpCall => {
                let callable = self.pop();
                self.call_value(&callable)?;
            }

            OpCode::OpTailCall => {
                let callable = self.pop();
                self.tail_call_value(callable)?;
            }

            OpCode::OpGetUpvalue(upv_idx) => {
                let value = self.frame().upvalue(upv_idx).clone();
                self.push(value);
            }

            OpCode::OpClosure(idx) => {
                let blueprint = match &self.chunk()[idx] {
                    Value::Blueprint(lambda) => lambda.clone(),
                    _ => panic!("compiler bug: non-blueprint in blueprint slot"),
                };

                let upvalue_count = blueprint.upvalue_count;
                debug_assert!(
                    upvalue_count > 0,
                    "OpClosure should not be called for plain lambdas"
                );

                let closure = Closure::new(blueprint);
                let upvalues = closure.upvalues_mut();
                self.push(Value::Closure(closure.clone()));

                // From this point on we internally mutate the
                // closure object's upvalues. The closure is
                // already in its stack slot, which means that it
                // can capture itself as an upvalue for
                // self-recursion.
                self.populate_upvalues(upvalue_count, upvalues)?;
            }

            OpCode::OpThunk(idx) => {
                let blueprint = match &self.chunk()[idx] {
                    Value::Blueprint(lambda) => lambda.clone(),
                    _ => panic!("compiler bug: non-blueprint in blueprint slot"),
                };

                let upvalue_count = blueprint.upvalue_count;
                let thunk = Thunk::new(blueprint, self.current_span());
                let upvalues = thunk.upvalues_mut();

                self.push(Value::Thunk(thunk.clone()));
                self.populate_upvalues(upvalue_count, upvalues)?;
            }

            OpCode::OpForce => {
                let mut value = self.pop();

                if let Value::Thunk(thunk) = value {
                    fallible!(self, thunk.force(self));
                    value = thunk.value().clone();
                }

                self.push(value);
            }

            OpCode::OpFinalise(StackIdx(idx)) => {
                match &self.stack[self.frame().stack_offset + idx] {
                    Value::Closure(closure) => {
                        closure.resolve_deferred_upvalues(&self.stack[self.frame().stack_offset..])
                    }

                    Value::Thunk(thunk) => {
                        thunk.resolve_deferred_upvalues(&self.stack[self.frame().stack_offset..])
                    }

                    // In functions with "formals" attributes, it is
                    // possible for `OpFinalise` to be called on a
                    // non-capturing value, in which case it is a no-op.
                    //
                    // TODO: detect this in some phase and skip the finalise; fail here
                    _ => { /* TODO: panic here again to catch bugs */ }
                }
            }

            // Data-carrying operands should never be executed,
            // that is a critical error in the VM.
            OpCode::DataLocalIdx(_)
            | OpCode::DataDeferredLocal(_)
            | OpCode::DataUpvalueIdx(_)
            | OpCode::DataCaptureWith => {
                panic!("VM bug: attempted to execute data-carrying operand")
            }
        }

        Ok(())
    }

    fn run_attrset(&mut self, count: usize) -> EvalResult<()> {
        let attrs = fallible!(
            self,
            NixAttrs::construct(count, self.stack.split_off(self.stack.len() - count * 2))
        );

        self.push(Value::attrs(attrs));
        Ok(())
    }

    /// Interpolate string fragments by popping the specified number of
    /// fragments of the stack, evaluating them to strings, and pushing
    /// the concatenated result string back on the stack.
    fn run_interpolate(&mut self, count: usize) -> EvalResult<()> {
        let mut out = String::new();

        for _ in 0..count {
            out.push_str(fallible!(self, self.pop().to_str()).as_str());
        }

        self.push(Value::String(out.into()));
        Ok(())
    }

    /// Resolve a dynamic identifier through the with-stack at runtime.
    fn resolve_with(&mut self, ident: &str) -> EvalResult<Value> {
        // Iterate over the with_stack manually to avoid borrowing
        // self, which is required for forcing the set.
        for with_stack_idx in (0..self.with_stack.len()).rev() {
            let with = self.stack[self.with_stack[with_stack_idx]].clone();

            if let Value::Thunk(thunk) = &with {
                fallible!(self, thunk.force(self));
            }

            match fallible!(self, with.to_attrs()).select(ident) {
                None => continue,
                Some(val) => return Ok(val.clone()),
            }
        }

        // Iterate over the captured with stack if one exists. This is
        // extra tricky to do without a lot of cloning.
        for idx in (0..self.frame().upvalues.with_stack_len()).rev() {
            // This is safe because having an index here guarantees
            // that the stack is present.
            let with =
                unsafe { self.frame().upvalues.with_stack().unwrap_unchecked()[idx].clone() };

            if let Value::Thunk(thunk) = &with {
                fallible!(self, thunk.force(self));
            }

            match fallible!(self, with.to_attrs()).select(ident) {
                None => continue,
                Some(val) => return Ok(val.clone()),
            }
        }

        Err(self.error(ErrorKind::UnknownDynamicVariable(ident.to_string())))
    }

    /// Populate the upvalue fields of a thunk or closure under construction.
    fn populate_upvalues(
        &mut self,
        count: usize,
        mut upvalues: RefMut<'_, Upvalues>,
    ) -> EvalResult<()> {
        for _ in 0..count {
            match self.inc_ip() {
                OpCode::DataLocalIdx(StackIdx(local_idx)) => {
                    let idx = self.frame().stack_offset + local_idx;
                    upvalues.push(self.stack[idx].clone());
                }

                OpCode::DataUpvalueIdx(upv_idx) => {
                    upvalues.push(self.frame().upvalue(upv_idx).clone());
                }

                OpCode::DataDeferredLocal(idx) => {
                    upvalues.push(Value::DeferredUpvalue(idx));
                }

                OpCode::DataCaptureWith => {
                    // Start the captured with_stack off of the
                    // current call frame's captured with_stack, ...
                    let mut captured_with_stack = self
                        .frame()
                        .upvalues
                        .with_stack()
                        .map(Clone::clone)
                        // ... or make an empty one if there isn't one already.
                        .unwrap_or_else(|| Vec::with_capacity(self.with_stack.len()));

                    for idx in &self.with_stack {
                        captured_with_stack.push(self.stack[*idx].clone());
                    }

                    upvalues.set_with_stack(captured_with_stack);
                }

                _ => panic!("compiler error: missing closure operand"),
            }
        }

        Ok(())
    }

    /// Strictly evaluate the supplied value for outputting it. This
    /// will ensure that lists and attribute sets do not contain
    /// chunks which, for users, are displayed in a strange and often
    /// unexpected way.
    fn force_for_output(&mut self, value: &Value) -> EvalResult<()> {
        match value {
            Value::Attrs(attrs) => {
                for (_, value) in attrs.iter() {
                    self.force_for_output(value)?;
                }
                Ok(())
            }

            Value::List(list) => list.iter().try_for_each(|elem| self.force_for_output(elem)),

            Value::Thunk(thunk) => {
                fallible!(self, thunk.force(self));
                let value = thunk.value().clone();
                self.force_for_output(&value)
            }

            // If any of these internal values are encountered here a
            // critical error has happened (likely a compiler bug).
            Value::AttrNotFound
            | Value::DynamicUpvalueMissing(_)
            | Value::Blueprint(_)
            | Value::DeferredUpvalue(_) => {
                panic!("tvix bug: internal value left on stack: {:?}", value)
            }

            Value::Null
            | Value::Bool(_)
            | Value::Integer(_)
            | Value::Float(_)
            | Value::String(_)
            | Value::Path(_)
            | Value::Closure(_)
            | Value::Builtin(_) => Ok(()),
        }
    }

    pub fn call_builtin(&mut self, builtin: Builtin) -> EvalResult<()> {
        let builtin_name = builtin.name();
        self.observer.observe_enter_builtin(builtin_name);

        let arg = self.pop();
        let result = fallible!(self, builtin.apply(self, arg));

        self.observer
            .observe_exit_builtin(builtin_name, &self.stack);

        self.push(result);

        Ok(())
    }
}

// TODO: use Rc::unwrap_or_clone once it is stabilised.
// https://doc.rust-lang.org/std/rc/struct.Rc.html#method.unwrap_or_clone
fn unwrap_or_clone_rc<T: Clone>(rc: Rc<T>) -> T {
    Rc::try_unwrap(rc).unwrap_or_else(|rc| (*rc).clone())
}

pub fn run_lambda(
    nix_search_path: NixSearchPath,
    observer: &mut dyn RuntimeObserver,
    lambda: Rc<Lambda>,
) -> EvalResult<RuntimeResult> {
    let mut vm = VM::new(nix_search_path, observer);
    vm.enter_frame(lambda, Upvalues::with_capacity(0), 0)?;
    let value = vm.pop();
    vm.force_for_output(&value)?;

    Ok(RuntimeResult {
        value,
        warnings: vm.warnings,
    })
}
