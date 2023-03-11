//! This module implements the abstract/virtual machine that runs Tvix
//! bytecode.
//!
//! The operation of the VM is facilitated by the [`Frame`] type,
//! which controls the current execution state of the VM and is
//! processed within the VM's operating loop.
//!
//! A [`VM`] is used by instantiating it with an initial [`Frame`],
//! then triggering its execution and waiting for the VM to return or
//! yield an error.

pub mod generators;
mod macros;

use serde_json::json;
use std::{cmp::Ordering, collections::HashMap, ops::DerefMut, path::PathBuf, rc::Rc};

use crate::{
    arithmetic_op,
    chunk::Chunk,
    cmp_op,
    compiler::GlobalsMap,
    errors::{Error, ErrorKind, EvalResult},
    io::EvalIO,
    nix_search_path::NixSearchPath,
    observer::RuntimeObserver,
    opcode::{CodeIdx, Count, JumpOffset, OpCode, StackIdx, UpvalueIdx},
    spans::LightSpan,
    upvalues::Upvalues,
    value::{
        Builtin, BuiltinResult, Closure, CoercionKind, Lambda, NixAttrs, NixList, PointerEquality,
        SharedThunkSet, Thunk, Value,
    },
    vm::generators::GenCo,
    warnings::{EvalWarning, WarningKind},
};

use generators::{call_functor, Generator, GeneratorState};

use self::generators::{GeneratorRequest, GeneratorResponse};

/// Internal helper trait for ergonomically converting from a `Result<T,
/// ErrorKind>` to a `Result<T, Error>` using the current span of a call frame.
trait WithSpan<T> {
    fn with_span(self, frame: &CallFrame) -> Result<T, Error>;
}

impl<T> WithSpan<T> for Result<T, ErrorKind> {
    fn with_span(self, frame: &CallFrame) -> Result<T, Error> {
        self.map_err(|kind| frame.error(kind))
    }
}

struct CallFrame {
    /// The lambda currently being executed.
    lambda: Rc<Lambda>,

    /// Optional captured upvalues of this frame (if a thunk or
    /// closure if being evaluated).
    upvalues: Rc<Upvalues>,

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

    /// Borrow the chunk of this frame's lambda.
    fn chunk(&self) -> &Chunk {
        &self.lambda.chunk
    }

    /// Increment this frame's instruction pointer and return the operation that
    /// the pointer moved past.
    fn inc_ip(&mut self) -> OpCode {
        let op = self.chunk()[self.ip];
        self.ip += 1;
        op
    }

    /// Construct an error from the given ErrorKind and the source span of the
    /// current instruction.
    pub fn error(&self, kind: ErrorKind) -> Error {
        Error::new(kind, self.chunk().get_span(self.ip - 1))
    }

    /// Returns the information needed to calculate the current span,
    /// but without performing that calculation.
    // TODO: why pub?
    pub(crate) fn current_light_span(&self) -> LightSpan {
        LightSpan::new_delayed(self.lambda.clone(), self.ip - 1)
    }
}

/// A frame represents an execution state of the VM. The VM has a stack of
/// frames representing the nesting of execution inside of the VM, and operates
/// on the frame at the top.
///
/// When a frame has been fully executed, it is removed from the VM's frame
/// stack and expected to leave a result [`Value`] on the top of the stack.
enum Frame {
    /// CallFrame represents the execution of Tvix bytecode within a thunk,
    /// function or closure.
    CallFrame {
        /// The call frame itself, separated out into another type to pass it
        /// around easily.
        call_frame: CallFrame,

        /// Span from which the call frame was launched.
        span: LightSpan,
    },

    /// Generator represents a frame that can yield further
    /// instructions to the VM while its execution is being driven.
    ///
    /// A generator is essentially an asynchronous function that can
    /// be suspended while waiting for the VM to do something (e.g.
    /// thunk forcing), and resume at the same point.
    Generator {
        /// human-readable description of the generator,
        name: &'static str,

        /// Span from which the generator was launched.
        span: LightSpan,

        state: GeneratorState,

        /// Generator itself, which can be resumed with `.resume()`.
        generator: Generator,
    },
}

impl Frame {
    pub fn span(&self) -> LightSpan {
        match self {
            Frame::CallFrame { span, .. } | Frame::Generator { span, .. } => span.clone(),
        }
    }
}

struct VM<'o> {
    /// VM's frame stack, representing the execution contexts the VM is working
    /// through. Elements are usually pushed when functions are called, or
    /// thunks are being forced.
    frames: Vec<Frame>,

    /// The VM's top-level value stack. Within this stack, each code-executing
    /// frame holds a "view" of the stack representing the slice of the
    /// top-level stack that is relevant to its operation. This is done to avoid
    /// allocating a new `Vec` for each frame's stack.
    pub(crate) stack: Vec<Value>,

    /// Stack indices (absolute indexes into `stack`) of attribute
    /// sets from which variables should be dynamically resolved
    /// (`with`).
    with_stack: Vec<usize>,

    /// Runtime warnings collected during evaluation.
    warnings: Vec<EvalWarning>,

    /// Import cache, mapping absolute file paths to the value that
    /// they compile to. Note that this reuses thunks, too!
    // TODO: should probably be based on a file hash
    pub import_cache: Box<HashMap<PathBuf, Value>>,

    /// Parsed Nix search path, which is used to resolve `<...>`
    /// references.
    nix_search_path: NixSearchPath,

    /// Implementation of I/O operations used for impure builtins and
    /// features like `import`.
    io_handle: Box<dyn EvalIO>,

    /// Runtime observer which can print traces of runtime operations.
    observer: &'o mut dyn RuntimeObserver,

    /// Strong reference to the globals, guaranteeing that they are
    /// kept alive for the duration of evaluation.
    ///
    /// This is important because recursive builtins (specifically
    /// `import`) hold a weak reference to the builtins, while the
    /// original strong reference is held by the compiler which does
    /// not exist anymore at runtime.
    #[allow(dead_code)]
    globals: Rc<GlobalsMap>,

    /// A reasonably applicable span that can be used for errors in each
    /// execution situation.
    ///
    /// The VM should update this whenever control flow changes take place (i.e.
    /// entering or exiting a frame to yield control somewhere).
    reasonable_span: LightSpan,

    /// This field is responsible for handling `builtins.tryEval`. When that
    /// builtin is encountered, it sends a special message to the VM which
    /// pushes the frame index that requested to be informed of catchable
    /// errors in this field.
    ///
    /// The frame stack is then laid out like this:
    ///
    /// ```notrust
    /// ┌──┬──────────────────────────┐
    /// │ 0│ `Result`-producing frame │
    /// ├──┼──────────────────────────┤
    /// │-1│ `builtins.tryEval` frame │
    /// ├──┼──────────────────────────┤
    /// │..│ ... other frames ...     │
    /// └──┴──────────────────────────┘
    /// ```
    ///
    /// Control is yielded to the outer VM loop, which evaluates the next frame
    /// and returns the result itself to the `builtins.tryEval` frame.
    try_eval_frames: Vec<usize>,
}

impl<'o> VM<'o> {
    pub fn new(
        nix_search_path: NixSearchPath,
        io_handle: Box<dyn EvalIO>,
        observer: &'o mut dyn RuntimeObserver,
        globals: Rc<GlobalsMap>,
        reasonable_span: LightSpan,
    ) -> Self {
        // Backtrace-on-stack-overflow is some seriously weird voodoo and
        // very unsafe.  This double-guard prevents it from accidentally
        // being enabled on release builds.
        #[cfg(debug_assertions)]
        #[cfg(feature = "backtrace_overflow")]
        unsafe {
            backtrace_on_stack_overflow::enable();
        };

        Self {
            nix_search_path,
            io_handle,
            observer,
            globals,
            reasonable_span,
            frames: vec![],
            stack: vec![],
            with_stack: vec![],
            warnings: vec![],
            import_cache: Default::default(),
            try_eval_frames: vec![],
        }
    }

    /// Push a call frame onto the frame stack.
    fn push_call_frame(&mut self, span: LightSpan, call_frame: CallFrame) {
        self.frames.push(Frame::CallFrame { span, call_frame })
    }

    /// Run the VM's primary (outer) execution loop, continuing execution based
    /// on the current frame at the top of the frame stack.
    fn execute(mut self) -> EvalResult<RuntimeResult> {
        let mut catchable_error_occurred = false;

        while let Some(frame) = self.frames.pop() {
            self.reasonable_span = frame.span();
            let frame_id = self.frames.len();

            match frame {
                Frame::CallFrame { call_frame, span } => {
                    self.observer
                        .observe_enter_call_frame(0, &call_frame.lambda, frame_id);

                    match self.execute_bytecode(span, call_frame) {
                        Ok(true) => self.observer.observe_exit_call_frame(frame_id, &self.stack),
                        Ok(false) => self
                            .observer
                            .observe_suspend_call_frame(frame_id, &self.stack),

                        Err(err) => {
                            if let Some(catching_frame_idx) = self.try_eval_frames.pop() {
                                if err.kind.is_catchable() {
                                    self.observer.observe_exit_call_frame(frame_id, &self.stack);
                                    catchable_error_occurred = true;

                                    // truncate the frame stack back to the
                                    // frame that can catch this error
                                    self.frames.truncate(/* len = */ catching_frame_idx + 1);
                                    continue;
                                }
                            }

                            return Err(err);
                        }
                    };
                }

                // Handle generator frames, which can request thunk forcing
                // during their execution.
                Frame::Generator {
                    name,
                    span,
                    state,
                    generator,
                } => {
                    self.observer
                        .observe_enter_generator(frame_id, name, &self.stack);

                    let initial_msg = if catchable_error_occurred {
                        catchable_error_occurred = false;
                        Some(GeneratorResponse::ForceError)
                    } else {
                        None
                    };

                    match self.run_generator(name, span, frame_id, state, generator, initial_msg) {
                        Ok(true) => {
                            self.observer
                                .observe_exit_generator(frame_id, name, &self.stack)
                        }
                        Ok(false) => {
                            self.observer
                                .observe_suspend_generator(frame_id, name, &self.stack)
                        }

                        Err(err) => {
                            if let Some(catching_frame_idx) = self.try_eval_frames.pop() {
                                if err.kind.is_catchable() {
                                    self.observer.observe_exit_generator(
                                        frame_id,
                                        name,
                                        &self.stack,
                                    );
                                    catchable_error_occurred = true;

                                    // truncate the frame stack back to the
                                    // frame that can catch this error
                                    self.frames.truncate(/* len = */ catching_frame_idx + 1);
                                    continue;
                                }
                            }

                            return Err(err);
                        }
                    };
                }
            }
        }

        // Once no more frames are present, return the stack's top value as the
        // result.
        Ok(RuntimeResult {
            value: self
                .stack
                .pop()
                .expect("tvix bug: runtime stack empty after execution"),

            warnings: self.warnings,
        })
    }

    /// Run the VM's inner execution loop, processing Tvix bytecode from a
    /// chunk. This function returns if:
    ///
    /// 1. The code has run to the end, and has left a value on the top of the
    ///    stack. In this case, the frame is not returned to the frame stack.
    ///
    /// 2. The code encounters a generator, in which case the frame in its
    /// current state is pushed back on the stack, and the generator is left on
    /// top of it for the outer loop to execute.
    ///
    /// 3. An error is encountered.
    ///
    /// This function *must* ensure that it leaves the frame stack in the
    /// correct order, especially when re-enqueuing a frame to execute.
    ///
    /// The return value indicates whether the bytecode has been executed to
    /// completion, or whether it has been suspended in favour of a generator.
    fn execute_bytecode(&mut self, span: LightSpan, mut frame: CallFrame) -> EvalResult<bool> {
        loop {
            let op = frame.inc_ip();
            self.observer.observe_execute_op(frame.ip, &op, &self.stack);

            // TODO: might be useful to reorder ops with most frequent ones first
            match op {
                OpCode::OpThunkSuspended(idx) | OpCode::OpThunkClosure(idx) => {
                    let blueprint = match &frame.chunk()[idx] {
                        Value::Blueprint(lambda) => lambda.clone(),
                        _ => panic!("compiler bug: non-blueprint in blueprint slot"),
                    };

                    let upvalue_count = blueprint.upvalue_count;
                    let thunk = if matches!(op, OpCode::OpThunkClosure(_)) {
                        debug_assert!(
                            upvalue_count > 0,
                            "OpThunkClosure should not be called for plain lambdas"
                        );
                        Thunk::new_closure(blueprint)
                    } else {
                        Thunk::new_suspended(blueprint, frame.current_light_span())
                    };
                    let upvalues = thunk.upvalues_mut();
                    self.stack.push(Value::Thunk(thunk.clone()));

                    // From this point on we internally mutate the
                    // upvalues. The closure (if `is_closure`) is
                    // already in its stack slot, which means that it
                    // can capture itself as an upvalue for
                    // self-recursion.
                    self.populate_upvalues(&mut frame, upvalue_count, upvalues)?;
                }

                OpCode::OpForce => {
                    if let Some(Value::Thunk(_)) = self.stack.last() {
                        let thunk = match self.stack_pop() {
                            Value::Thunk(t) => t,
                            _ => unreachable!(),
                        };

                        let gen_span = frame.current_light_span();

                        self.push_call_frame(span, frame);
                        self.enqueue_generator("force", gen_span, |co| thunk.force(co));
                        return Ok(false);
                    }
                }

                OpCode::OpGetUpvalue(upv_idx) => {
                    let value = frame.upvalue(upv_idx).clone();
                    self.stack.push(value);
                }

                // Discard the current frame.
                OpCode::OpReturn => {
                    return Ok(true);
                }

                OpCode::OpConstant(idx) => {
                    let c = frame.chunk()[idx].clone();
                    self.stack.push(c);
                }

                OpCode::OpCall => {
                    let callable = self.stack_pop();
                    self.call_value(frame.current_light_span(), Some(frame), callable)?;

                    // exit this loop and let the outer loop enter the new call
                    return Ok(true);
                }

                // Remove the given number of elements from the stack,
                // but retain the top value.
                OpCode::OpCloseScope(Count(count)) => {
                    // Immediately move the top value into the right
                    // position.
                    let target_idx = self.stack.len() - 1 - count;
                    self.stack[target_idx] = self.stack_pop();

                    // Then drop the remaining values.
                    for _ in 0..(count - 1) {
                        self.stack.pop();
                    }
                }

                OpCode::OpClosure(idx) => {
                    let blueprint = match &frame.chunk()[idx] {
                        Value::Blueprint(lambda) => lambda.clone(),
                        _ => panic!("compiler bug: non-blueprint in blueprint slot"),
                    };

                    let upvalue_count = blueprint.upvalue_count;
                    debug_assert!(
                        upvalue_count > 0,
                        "OpClosure should not be called for plain lambdas"
                    );

                    let mut upvalues = Upvalues::with_capacity(blueprint.upvalue_count);
                    self.populate_upvalues(&mut frame, upvalue_count, &mut upvalues)?;
                    self.stack
                        .push(Value::Closure(Rc::new(Closure::new_with_upvalues(
                            Rc::new(upvalues),
                            blueprint,
                        ))));
                }

                OpCode::OpAttrsSelect => {
                    let key = self.stack_pop().to_str().with_span(&frame)?;
                    let attrs = self.stack_pop().to_attrs().with_span(&frame)?;

                    match attrs.select(key.as_str()) {
                        Some(value) => self.stack.push(value.clone()),

                        None => {
                            return Err(frame.error(ErrorKind::AttributeNotFound {
                                name: key.as_str().to_string(),
                            }))
                        }
                    }
                }

                OpCode::OpJumpIfFalse(JumpOffset(offset)) => {
                    debug_assert!(offset != 0);
                    if !self.stack_peek(0).as_bool().with_span(&frame)? {
                        frame.ip += offset;
                    }
                }

                OpCode::OpPop => {
                    self.stack.pop();
                }

                OpCode::OpAttrsTrySelect => {
                    let key = self.stack_pop().to_str().with_span(&frame)?;
                    let value = match self.stack_pop() {
                        Value::Attrs(attrs) => match attrs.select(key.as_str()) {
                            Some(value) => value.clone(),
                            None => Value::AttrNotFound,
                        },

                        _ => Value::AttrNotFound,
                    };

                    self.stack.push(value);
                }

                OpCode::OpGetLocal(StackIdx(local_idx)) => {
                    let idx = frame.stack_offset + local_idx;
                    self.stack.push(self.stack[idx].clone());
                }

                OpCode::OpJumpIfNotFound(JumpOffset(offset)) => {
                    debug_assert!(offset != 0);
                    if matches!(self.stack_peek(0), Value::AttrNotFound) {
                        self.stack_pop();
                        frame.ip += offset;
                    }
                }

                OpCode::OpJump(JumpOffset(offset)) => {
                    debug_assert!(offset != 0);
                    frame.ip += offset;
                }

                OpCode::OpEqual => {
                    let b = self.stack_pop();
                    let a = self.stack_pop();
                    let gen_span = frame.current_light_span();
                    self.push_call_frame(span, frame);
                    self.enqueue_generator("nix_eq", gen_span, |co| {
                        a.nix_eq(b, co, PointerEquality::ForbidAll)
                    });
                    return Ok(false);
                }

                // These assertion operations error out if the stack
                // top is not of the expected type. This is necessary
                // to implement some specific behaviours of Nix
                // exactly.
                OpCode::OpAssertBool => {
                    let val = self.stack_peek(0);
                    if !val.is_bool() {
                        return Err(frame.error(ErrorKind::TypeError {
                            expected: "bool",
                            actual: val.type_of(),
                        }));
                    }
                }

                OpCode::OpAttrs(Count(count)) => self.run_attrset(&frame, count)?,

                OpCode::OpAttrsUpdate => {
                    let rhs = self.stack_pop().to_attrs().with_span(&frame)?;
                    let lhs = self.stack_pop().to_attrs().with_span(&frame)?;

                    self.stack.push(Value::attrs(lhs.update(*rhs)))
                }

                OpCode::OpInvert => {
                    let v = self.stack_pop().as_bool().with_span(&frame)?;
                    self.stack.push(Value::Bool(!v));
                }

                OpCode::OpList(Count(count)) => {
                    let list =
                        NixList::construct(count, self.stack.split_off(self.stack.len() - count));

                    self.stack.push(Value::List(list));
                }

                OpCode::OpJumpIfTrue(JumpOffset(offset)) => {
                    debug_assert!(offset != 0);
                    if self.stack_peek(0).as_bool().with_span(&frame)? {
                        frame.ip += offset;
                    }
                }

                OpCode::OpHasAttr => {
                    let key = self.stack_pop().to_str().with_span(&frame)?;
                    let result = match self.stack_pop() {
                        Value::Attrs(attrs) => attrs.contains(key.as_str()),

                        // Nix allows use of `?` on non-set types, but
                        // always returns false in those cases.
                        _ => false,
                    };

                    self.stack.push(Value::Bool(result));
                }

                OpCode::OpConcat => {
                    let rhs = self.stack_pop().to_list().with_span(&frame)?.into_inner();
                    let lhs = self.stack_pop().to_list().with_span(&frame)?.into_inner();
                    self.stack.push(Value::List(NixList::from(lhs + rhs)))
                }

                OpCode::OpResolveWith => {
                    let ident = self.stack_pop().to_str().with_span(&frame)?;

                    // Re-enqueue this frame.
                    let op_span = frame.current_light_span();
                    self.push_call_frame(span, frame);

                    // Construct a generator frame doing the lookup in constant
                    // stack space.
                    let with_stack_len = self.with_stack.len();
                    let closed_with_stack_len = self
                        .last_call_frame()
                        .map(|frame| frame.upvalues.with_stack_len())
                        .unwrap_or(0);

                    self.enqueue_generator("resolve_with", op_span, |co| {
                        resolve_with(
                            co,
                            ident.as_str().to_owned(),
                            with_stack_len,
                            closed_with_stack_len,
                        )
                    });

                    return Ok(false);
                }

                OpCode::OpFinalise(StackIdx(idx)) => {
                    match &self.stack[frame.stack_offset + idx] {
                        Value::Closure(_) => panic!("attempted to finalise a closure"),
                        Value::Thunk(thunk) => thunk.finalise(&self.stack[frame.stack_offset..]),

                        // In functions with "formals" attributes, it is
                        // possible for `OpFinalise` to be called on a
                        // non-capturing value, in which case it is a no-op.
                        //
                        // TODO: detect this in some phase and skip the finalise; fail here
                        _ => { /* TODO: panic here again to catch bugs */ }
                    }
                }

                OpCode::OpCoerceToString => {
                    let value = self.stack_pop();
                    let gen_span = frame.current_light_span();
                    self.push_call_frame(span, frame);

                    self.enqueue_generator("coerce_to_string", gen_span, |co| {
                        value.coerce_to_string(co, CoercionKind::Weak)
                    });

                    return Ok(false);
                }

                OpCode::OpInterpolate(Count(count)) => self.run_interpolate(&frame, count)?,

                OpCode::OpValidateClosedFormals => {
                    let formals = frame.lambda.formals.as_ref().expect(
                        "OpValidateClosedFormals called within the frame of a lambda without formals",
                    );

                    let args = self.stack_peek(0).to_attrs().with_span(&frame)?;
                    for arg in args.keys() {
                        if !formals.contains(arg) {
                            return Err(frame.error(ErrorKind::UnexpectedArgument {
                                arg: arg.clone(),
                                formals_span: formals.span,
                            }));
                        }
                    }
                }

                OpCode::OpAdd => {
                    let b = self.stack_pop();
                    let a = self.stack_pop();

                    let gen_span = frame.current_light_span();
                    self.push_call_frame(span, frame);

                    // OpAdd can add not just numbers, but also string-like
                    // things, which requires more VM logic. This operation is
                    // evaluated in a generator frame.
                    self.enqueue_generator("add_values", gen_span, |co| add_values(co, a, b));
                    return Ok(false);
                }

                OpCode::OpSub => {
                    let b = self.stack_pop();
                    let a = self.stack_pop();
                    let result = arithmetic_op!(&a, &b, -).with_span(&frame)?;
                    self.stack.push(result);
                }

                OpCode::OpMul => {
                    let b = self.stack_pop();
                    let a = self.stack_pop();
                    let result = arithmetic_op!(&a, &b, *).with_span(&frame)?;
                    self.stack.push(result);
                }

                OpCode::OpDiv => {
                    let b = self.stack_pop();

                    match b {
                        Value::Integer(0) => return Err(frame.error(ErrorKind::DivisionByZero)),
                        Value::Float(b) if b == 0.0_f64 => {
                            return Err(frame.error(ErrorKind::DivisionByZero))
                        }
                        _ => {}
                    };

                    let a = self.stack_pop();
                    let result = arithmetic_op!(&a, &b, /).with_span(&frame)?;
                    self.stack.push(result);
                }

                OpCode::OpNegate => match self.stack_pop() {
                    Value::Integer(i) => self.stack.push(Value::Integer(-i)),
                    Value::Float(f) => self.stack.push(Value::Float(-f)),
                    v => {
                        return Err(frame.error(ErrorKind::TypeError {
                            expected: "number (either int or float)",
                            actual: v.type_of(),
                        }));
                    }
                },

                OpCode::OpLess => cmp_op!(self, frame, span, <),
                OpCode::OpLessOrEq => cmp_op!(self, frame, span, <=),
                OpCode::OpMore => cmp_op!(self, frame, span, >),
                OpCode::OpMoreOrEq => cmp_op!(self, frame, span, >=),

                OpCode::OpFindFile => match self.stack_pop() {
                    Value::UnresolvedPath(path) => {
                        let resolved = self
                            .nix_search_path
                            .resolve(&*self.io_handle, *path)
                            .with_span(&frame)?;
                        self.stack.push(resolved.into());
                    }

                    _ => panic!("tvix compiler bug: OpFindFile called on non-UnresolvedPath"),
                },

                OpCode::OpResolveHomePath => match self.stack_pop() {
                    Value::UnresolvedPath(path) => {
                        match dirs::home_dir() {
                            None => {
                                return Err(frame.error(ErrorKind::RelativePathResolution(
                                    "failed to determine home directory".into(),
                                )));
                            }
                            Some(mut buf) => {
                                buf.push(*path);
                                self.stack.push(buf.into());
                            }
                        };
                    }

                    _ => {
                        panic!("tvix compiler bug: OpResolveHomePath called on non-UnresolvedPath")
                    }
                },

                OpCode::OpPushWith(StackIdx(idx)) => self.with_stack.push(frame.stack_offset + idx),

                OpCode::OpPopWith => {
                    self.with_stack.pop();
                }

                OpCode::OpAssertFail => {
                    return Err(frame.error(ErrorKind::AssertionFailed));
                }

                // Data-carrying operands should never be executed,
                // that is a critical error in the VM/compiler.
                OpCode::DataStackIdx(_)
                | OpCode::DataDeferredLocal(_)
                | OpCode::DataUpvalueIdx(_)
                | OpCode::DataCaptureWith => {
                    panic!("Tvix bug: attempted to execute data-carrying operand")
                }
            }
        }
    }
}

/// Implementation of helper functions for the runtime logic above.
impl<'o> VM<'o> {
    pub(crate) fn stack_pop(&mut self) -> Value {
        self.stack.pop().expect("runtime stack empty")
    }

    fn stack_peek(&self, offset: usize) -> &Value {
        &self.stack[self.stack.len() - 1 - offset]
    }

    fn run_attrset(&mut self, frame: &CallFrame, count: usize) -> EvalResult<()> {
        let attrs = NixAttrs::construct(count, self.stack.split_off(self.stack.len() - count * 2))
            .with_span(frame)?;

        self.stack.push(Value::attrs(attrs));
        Ok(())
    }

    /// Access the last call frame present in the frame stack.
    fn last_call_frame(&self) -> Option<&CallFrame> {
        for frame in self.frames.iter().rev() {
            if let Frame::CallFrame { call_frame, .. } = frame {
                return Some(call_frame);
            }
        }

        None
    }

    /// Push an already constructed warning.
    pub fn push_warning(&mut self, warning: EvalWarning) {
        self.warnings.push(warning);
    }

    /// Emit a warning with the given WarningKind and the source span
    /// of the current instruction.
    pub fn emit_warning(&mut self, _kind: WarningKind) {
        // TODO: put LightSpan in warning, calculate only *after* eval
        // TODO: what to do with the spans?
        // self.push_warning(EvalWarning {
        //     kind,
        //     span: self.current_span(),
        // });
    }

    /// Interpolate string fragments by popping the specified number of
    /// fragments of the stack, evaluating them to strings, and pushing
    /// the concatenated result string back on the stack.
    fn run_interpolate(&mut self, frame: &CallFrame, count: usize) -> EvalResult<()> {
        let mut out = String::new();

        for _ in 0..count {
            out.push_str(self.stack_pop().to_str().with_span(frame)?.as_str());
        }

        self.stack.push(Value::String(out.into()));
        Ok(())
    }

    /// Returns a reasonable light span for the current situation that the VM is
    /// in.
    pub fn reasonable_light_span(&self) -> LightSpan {
        self.reasonable_span.clone()
    }

    /// Construct an error from the given ErrorKind and the source
    /// span of the current instruction.
    pub fn error(&self, kind: ErrorKind) -> Error {
        Error::new(kind, self.reasonable_span.span())
    }

    /// Apply an argument from the stack to a builtin, and attempt to call it.
    ///
    /// All calls are tail-calls in Tvix, as every function application is a
    /// separate thunk and OpCall is thus the last result in the thunk.
    ///
    /// Due to this, once control flow exits this function, the generator will
    /// automatically be run by the VM.
    fn call_builtin(&mut self, span: LightSpan, mut builtin: Builtin) -> EvalResult<()> {
        let builtin_name = builtin.name();
        self.observer.observe_enter_builtin(builtin_name);

        builtin.apply_arg(self.stack_pop());

        match builtin.call() {
            // Partially applied builtin is just pushed back on the stack.
            BuiltinResult::Partial(partial) => self.stack.push(Value::Builtin(partial)),

            // Builtin is fully applied and the generator needs to be run by the VM.
            BuiltinResult::Called(name, generator) => self.frames.push(Frame::Generator {
                generator,
                span,
                name,
                state: GeneratorState::Running,
            }),
        }

        Ok(())
    }

    fn call_value(
        &mut self,
        span: LightSpan,
        parent: Option<CallFrame>,
        callable: Value,
    ) -> EvalResult<()> {
        match callable {
            Value::Builtin(builtin) => self.call_builtin(span, builtin),
            Value::Thunk(thunk) => self.call_value(span, parent, thunk.value().clone()),

            Value::Closure(closure) => {
                let lambda = closure.lambda();
                self.observer.observe_tail_call(self.frames.len(), &lambda);

                // The stack offset is always `stack.len() - arg_count`, and
                // since this branch handles native Nix functions (which always
                // take only a single argument and are curried), the offset is
                // `stack_len - 1`.
                let stack_offset = self.stack.len() - 1;

                self.push_call_frame(
                    span,
                    CallFrame {
                        lambda,
                        upvalues: closure.upvalues(),
                        ip: CodeIdx(0),
                        stack_offset,
                    },
                );

                Ok(())
            }

            // Attribute sets with a __functor attribute are callable.
            val @ Value::Attrs(_) => {
                let gen_span = parent
                    .map(|p| p.current_light_span())
                    .unwrap_or_else(|| self.reasonable_light_span());

                self.enqueue_generator("__functor call", gen_span, |co| call_functor(co, val));
                Ok(())
            }
            v => Err(self.error(ErrorKind::NotCallable(v.type_of()))),
        }
    }

    /// Populate the upvalue fields of a thunk or closure under construction.
    fn populate_upvalues(
        &mut self,
        frame: &mut CallFrame,
        count: usize,
        mut upvalues: impl DerefMut<Target = Upvalues>,
    ) -> EvalResult<()> {
        for _ in 0..count {
            match frame.inc_ip() {
                OpCode::DataStackIdx(StackIdx(stack_idx)) => {
                    let idx = frame.stack_offset + stack_idx;

                    let val = match self.stack.get(idx) {
                        Some(val) => val.clone(),
                        None => {
                            return Err(frame.error(ErrorKind::TvixBug {
                                msg: "upvalue to be captured was missing on stack",
                                metadata: Some(Rc::new(json!({
                                    "ip": format!("{:#x}", frame.ip.0 - 1),
                                    "stack_idx(relative)": stack_idx,
                                    "stack_idx(absolute)": idx,
                                }))),
                            }))
                        }
                    };

                    upvalues.deref_mut().push(val);
                }

                OpCode::DataUpvalueIdx(upv_idx) => {
                    upvalues.deref_mut().push(frame.upvalue(upv_idx).clone());
                }

                OpCode::DataDeferredLocal(idx) => {
                    upvalues.deref_mut().push(Value::DeferredUpvalue(idx));
                }

                OpCode::DataCaptureWith => {
                    // Start the captured with_stack off of the
                    // current call frame's captured with_stack, ...
                    let mut captured_with_stack = frame
                        .upvalues
                        .with_stack()
                        .map(Clone::clone)
                        // ... or make an empty one if there isn't one already.
                        .unwrap_or_else(|| Vec::with_capacity(self.with_stack.len()));

                    for idx in &self.with_stack {
                        captured_with_stack.push(self.stack[*idx].clone());
                    }

                    upvalues.deref_mut().set_with_stack(captured_with_stack);
                }

                _ => panic!("compiler error: missing closure operand"),
            }
        }

        Ok(())
    }
}

/// Fetch and force a value on the with-stack from the VM.
async fn fetch_forced_with(co: &GenCo, idx: usize) -> Value {
    match co.yield_(GeneratorRequest::WithValue(idx)).await {
        GeneratorResponse::Value(value) => value,
        msg => panic!(
            "Tvix bug: VM responded with incorrect generator message: {}",
            msg
        ),
    }
}

/// Fetch and force a value on the *captured* with-stack from the VM.
async fn fetch_captured_with(co: &GenCo, idx: usize) -> Value {
    match co.yield_(GeneratorRequest::CapturedWithValue(idx)).await {
        GeneratorResponse::Value(value) => value,
        msg => panic!(
            "Tvix bug: VM responded with incorrect generator message: {}",
            msg
        ),
    }
}

/// Resolve a dynamically bound identifier (through `with`) by looking
/// for matching values in the with-stacks carried at runtime.
async fn resolve_with(
    co: GenCo,
    ident: String,
    vm_with_len: usize,
    upvalue_with_len: usize,
) -> Result<Value, ErrorKind> {
    for with_stack_idx in (0..vm_with_len).rev() {
        // TODO(tazjin): is this branch still live with the current with-thunking?
        let with = fetch_forced_with(&co, with_stack_idx).await;

        match with.to_attrs()?.select(&ident) {
            None => continue,
            Some(val) => return Ok(val.clone()),
        }
    }

    for upvalue_with_idx in (0..upvalue_with_len).rev() {
        let with = fetch_captured_with(&co, upvalue_with_idx).await;

        match with.to_attrs()?.select(&ident) {
            None => continue,
            Some(val) => return Ok(val.clone()),
        }
    }

    Err(ErrorKind::UnknownDynamicVariable(ident))
}

async fn add_values(co: GenCo, a: Value, b: Value) -> Result<Value, ErrorKind> {
    let result = match (a, b) {
        (Value::Path(p), v) => {
            let mut path = p.to_string_lossy().into_owned();
            let vs = generators::request_string_coerce(&co, v, CoercionKind::Weak).await;
            path.push_str(vs.as_str());
            crate::value::canon_path(PathBuf::from(path)).into()
        }
        (Value::String(s1), Value::String(s2)) => Value::String(s1.concat(&s2)),
        (Value::String(s1), v) => Value::String(
            s1.concat(&generators::request_string_coerce(&co, v, CoercionKind::Weak).await),
        ),
        (v, Value::String(s2)) => Value::String(
            generators::request_string_coerce(&co, v, CoercionKind::Weak)
                .await
                .concat(&s2),
        ),
        (a, b) => arithmetic_op!(&a, &b, +)?,
    };

    Ok(result)
}

/// The result of a VM's runtime evaluation.
pub struct RuntimeResult {
    pub value: Value,
    pub warnings: Vec<EvalWarning>,
}

/// Generator that retrieves the final value from the stack, and deep-forces it
/// before returning.
async fn final_deep_force(co: GenCo) -> Result<Value, ErrorKind> {
    let value = generators::request_stack_pop(&co).await;
    Ok(generators::request_deep_force(&co, value, SharedThunkSet::default()).await)
}

pub fn run_lambda(
    nix_search_path: NixSearchPath,
    io_handle: Box<dyn EvalIO>,
    observer: &mut dyn RuntimeObserver,
    globals: Rc<GlobalsMap>,
    lambda: Rc<Lambda>,
) -> EvalResult<RuntimeResult> {
    // Retain the top-level span of the expression in this lambda, as
    // synthetic "calls" in deep_force will otherwise not have a span
    // to fall back to.
    //
    // We exploit the fact that the compiler emits a final instruction
    // with the span of the entire file for top-level expressions.
    let root_span = lambda.chunk.get_span(CodeIdx(lambda.chunk.code.len() - 1));

    let mut vm = VM::new(
        nix_search_path,
        io_handle,
        observer,
        globals,
        root_span.into(),
    );

    // Synthesise a frame that will instruct the VM to deep-force the final
    // value before returning it.
    vm.enqueue_generator("final_deep_force", root_span.into(), final_deep_force);

    vm.frames.push(Frame::CallFrame {
        span: root_span.into(),
        call_frame: CallFrame {
            lambda,
            upvalues: Rc::new(Upvalues::with_capacity(0)),
            ip: CodeIdx(0),
            stack_offset: 0,
        },
    });

    vm.execute()
}
