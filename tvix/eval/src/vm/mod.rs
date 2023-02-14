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

use genawaiter::rc::Gen;
use serde_json::json;
use std::{
    cmp::Ordering, collections::HashMap, future::Future, ops::DerefMut, path::PathBuf, pin::Pin,
    rc::Rc,
};

use crate::{
    arithmetic_op,
    chunk::Chunk,
    cmp_op,
    compiler::GlobalsMap,
    errors::{Error, ErrorKind, EvalResult},
    fallible,
    io::EvalIO,
    nix_search_path::NixSearchPath,
    observer::RuntimeObserver,
    opcode::{CodeIdx, Count, JumpOffset, OpCode, StackIdx, UpvalueIdx},
    spans::LightSpan,
    upvalues::Upvalues,
    value::{Builtin, Closure, CoercionKind, Lambda, NixAttrs, NixList, Thunk, Value},
    vm::generators::GenCo,
    warnings::{EvalWarning, WarningKind},
};

use generators::{neo_resolve_with, pin_generator, Generator, GeneratorState};

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

    continuation: Option<Continuation>,
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

    /// Trampoline represents a special execution mode, in which the VM moves
    /// back-and-forth between executing logic higher up on the frame stack and
    /// instructions from the trampoline.
    ///
    /// When a trampoline is reached by the VM and has neither an action nor a
    /// continuation, it is removed from the frame stack and execution passes to
    /// its parent.
    Trampoline {
        /// Span from which the trampoline was launched.
        span: LightSpan,

        /// The action to perform upon return to the trampoline.
        action: Option<TrampolineAction>,

        /// The continuation to execute after the action has completed.
        continuation: Option<Continuation>,
    },

    /// Generator represents a frame that can yield further
    /// instructions to the VM while its execution is being driven.
    ///
    /// A generator is essentially an asynchronous function that can
    /// be suspended while waiting for the VM to do something (e.g.
    /// thunk forcing), and resume at the same point.
    // TODO: thunk forcing ... ok, but calling stuff? hmm.
    Generator {
        /// Span from which the generator was launched.
        span: LightSpan,

        state: GeneratorState,

        /// Generator itself, which can be resumed with `.resume()`.
        generator: Generator,
    },
}

pub struct VM<'o> {
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
}

impl<'o> VM<'o> {
    pub fn new(
        nix_search_path: NixSearchPath,
        io_handle: Box<dyn EvalIO>,
        observer: &'o mut dyn RuntimeObserver,
        globals: Rc<GlobalsMap>,
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
            frames: vec![],
            stack: vec![],
            with_stack: vec![],
            warnings: vec![],
            import_cache: Default::default(),
        }
    }

    /// Push a trampoline onto the frame stack, if it has an action or
    /// continuation. Does nothing if the trampoline is empty.
    fn push_trampoline(&mut self, span: LightSpan, trampoline: Trampoline) {
        if trampoline.action.is_none() && trampoline.continuation.is_none() {
            return;
        }

        self.frames.push(Frame::Trampoline {
            span,
            action: trampoline.action,
            continuation: trampoline.continuation,
        });
    }

    /// Push a call frame onto the frame stack.
    fn push_call_frame(&mut self, span: LightSpan, call_frame: CallFrame) {
        self.frames.push(Frame::CallFrame { span, call_frame })
    }

    /// Run the VM's primary (outer) execution loop, continuing execution based
    /// on the current frame at the top of the frame stack.
    fn execute(mut self) -> EvalResult<RuntimeResult> {
        while let Some(frame) = self.frames.pop() {
            match frame {
                Frame::CallFrame { call_frame, span } => self.execute_bytecode(span, call_frame)?,

                // Handle empty trampolines by just dropping them.
                Frame::Trampoline {
                    action: None,
                    continuation: None,
                    ..
                } => continue,

                // Handle trampolines with an action. The trampoline is left in
                // its stack slot with any potential continuation retained, so
                // that the VM can return to it after the action.
                Frame::Trampoline {
                    action: Some(action),
                    span,
                    continuation,
                } => {
                    match action {
                        TrampolineAction::EnterFrame {
                            lambda,
                            upvalues,
                            light_span,
                            arg_count,
                        } => {
                            self.frames.push(Frame::Trampoline {
                                span,
                                continuation,
                                action: None,
                            });

                            self.push_call_frame(
                                light_span,
                                CallFrame {
                                    lambda,
                                    upvalues,
                                    ip: CodeIdx(0),
                                    stack_offset: self.stack.len() - arg_count,
                                    continuation: None, // TODO(tazjin): remove
                                },
                            );
                        }

                        TrampolineAction::DeepForce => {
                            let value = self.stack_pop();
                            let gen_frame = Frame::Generator {
                                span,
                                state: GeneratorState::Running,
                                generator: Gen::new(|co| pin_generator(value.deep_force(co))),
                            };

                            self.frames.push(gen_frame);
                        }
                    }
                }

                // Handle trampolines with continuations.
                Frame::Trampoline {
                    span,
                    action: None,
                    continuation: Some(continuation),
                } => {
                    let trampoline = continuation(&mut self)?;

                    self.frames.push(Frame::Trampoline {
                        span,
                        action: trampoline.action,
                        continuation: trampoline.continuation,
                    });
                }

                // Handle generator frames, which can request thunk forcing
                // during their execution.
                Frame::Generator {
                    span,
                    state,
                    generator,
                } => self.run_generator(span, state, generator)?,
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
    /// 2. The code encounters a trampoline, in which case the frame in its
    ///    current state is pushed back on the stack, and a trampoline is left
    ///    on top of it for the outer loop to execute.
    ///
    /// 3. An error is encountered.
    ///
    /// This function *must* ensure that it leaves the frame stack in the
    /// correct order, especially when re-enqueuing a frame to execute.
    fn execute_bytecode(&mut self, span: LightSpan, mut frame: CallFrame) -> EvalResult<()> {
        loop {
            let op = frame.inc_ip();
            self.observer.observe_execute_op(frame.ip, &op, &self.stack);

            // TODO: might be useful to reorder ops with most frequent ones first
            match op {
                // Discard the current frame.
                OpCode::OpReturn => return Ok(()),

                OpCode::OpConstant(idx) => {
                    let c = frame.chunk()[idx].clone();
                    self.stack.push(c);
                }

                OpCode::OpPop => {
                    self.stack.pop();
                }

                OpCode::OpAdd => {
                    let b = self.stack_pop();
                    let a = self.stack_pop();

                    let result = match (&a, &b) {
                        (Value::Path(p), v) => {
                            let mut path = p.to_string_lossy().into_owned();
                            path.push_str(
                                &v.coerce_to_string(CoercionKind::Weak, self)
                                    .with_span(&frame)?,
                            );
                            crate::value::canon_path(PathBuf::from(path)).into()
                        }
                        (Value::String(s1), Value::String(s2)) => Value::String(s1.concat(s2)),
                        (Value::String(s1), v) => Value::String(
                            s1.concat(
                                &v.coerce_to_string(CoercionKind::Weak, self)
                                    .with_span(&frame)?,
                            ),
                        ),
                        (v, Value::String(s2)) => Value::String(
                            v.coerce_to_string(CoercionKind::Weak, self)
                                .with_span(&frame)?
                                .concat(s2),
                        ),
                        _ => arithmetic_op!(&a, &b, +).with_span(&frame)?,
                    };

                    self.stack.push(result)
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

                OpCode::OpInvert => {
                    let v = self.stack_pop().as_bool().with_span(&frame)?;
                    self.stack.push(Value::Bool(!v));
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

                OpCode::OpEqual => {
                    let b = self.stack_pop();
                    let a = self.stack_pop();

                    let gen_frame = Frame::Generator {
                        span: frame.current_light_span(),
                        state: GeneratorState::Running,
                        generator: Gen::new(|co| pin_generator(a.nix_eq(b, co, true))),
                    };

                    self.push_call_frame(span, frame);
                    self.frames.push(gen_frame);
                    return Ok(());
                }

                OpCode::OpLess => cmp_op!(self, frame, span, <),
                OpCode::OpLessOrEq => cmp_op!(self, frame, span, <=),
                OpCode::OpMore => cmp_op!(self, frame, span, >),
                OpCode::OpMoreOrEq => cmp_op!(self, frame, span, >=),

                OpCode::OpAttrs(Count(count)) => self.run_attrset(&frame, count)?,

                OpCode::OpAttrsUpdate => {
                    let rhs = self.stack_pop().to_attrs().with_span(&frame)?;
                    let lhs = self.stack_pop().to_attrs().with_span(&frame)?;

                    self.stack.push(Value::attrs(lhs.update(*rhs)))
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

                OpCode::OpList(Count(count)) => {
                    let list =
                        NixList::construct(count, self.stack.split_off(self.stack.len() - count));

                    self.stack.push(Value::List(list));
                }

                OpCode::OpConcat => {
                    let rhs = self.stack_pop().to_list().with_span(&frame)?.into_inner();
                    let lhs = self.stack_pop().to_list().with_span(&frame)?.into_inner();
                    self.stack.push(Value::List(NixList::from(lhs + rhs)))
                }

                OpCode::OpInterpolate(Count(count)) => self.run_interpolate(&frame, count)?,

                OpCode::OpCoerceToString => {
                    // TODO: handle string context, copying to store
                    let string = self
                        .stack_pop()
                        .coerce_to_string(CoercionKind::Weak, self)
                        .with_span(&frame)?;

                    self.stack.push(Value::String(string));
                }

                OpCode::OpFindFile => match self.stack_pop() {
                    Value::UnresolvedPath(path) => {
                        let resolved = self.nix_search_path.resolve(path).with_span(&frame)?;
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
                                buf.push(path);
                                self.stack.push(buf.into());
                            }
                        };
                    }

                    _ => {
                        panic!("tvix compiler bug: OpResolveHomePath called on non-UnresolvedPath")
                    }
                },

                OpCode::OpJump(JumpOffset(offset)) => {
                    debug_assert!(offset != 0);
                    frame.ip += offset;
                }

                OpCode::OpJumpIfTrue(JumpOffset(offset)) => {
                    debug_assert!(offset != 0);
                    if self.stack_peek(0).as_bool().with_span(&frame)? {
                        frame.ip += offset;
                    }
                }

                OpCode::OpJumpIfFalse(JumpOffset(offset)) => {
                    debug_assert!(offset != 0);
                    if !self.stack_peek(0).as_bool().with_span(&frame)? {
                        frame.ip += offset;
                    }
                }

                OpCode::OpJumpIfNotFound(JumpOffset(offset)) => {
                    debug_assert!(offset != 0);
                    if matches!(self.stack_peek(0), Value::AttrNotFound) {
                        self.stack_pop();
                        frame.ip += offset;
                    }
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

                OpCode::OpGetLocal(StackIdx(local_idx)) => {
                    let idx = frame.stack_offset + local_idx;
                    self.stack.push(self.stack[idx].clone());
                }

                OpCode::OpPushWith(StackIdx(idx)) => self.with_stack.push(frame.stack_offset + idx),

                OpCode::OpPopWith => {
                    self.with_stack.pop();
                }

                OpCode::OpResolveWith => {
                    let ident = self.stack_pop().to_str().with_span(&frame)?;

                    // Re-enqueue this frame.
                    let op_span = frame.current_light_span();
                    self.push_call_frame(span, frame);

                    // Construct a generator frame doing the lookup in constant
                    // stack space.
                    let gen_frame = Frame::Generator {
                        span: op_span,
                        state: GeneratorState::Running,
                        generator: Gen::new(|co| {
                            pin_generator(neo_resolve_with(
                                ident.as_str().to_owned(),
                                self.with_stack.len(),
                                self.last_call_frame()
                                    .map(|frame| frame.upvalues.with_stack_len())
                                    .unwrap_or(0),
                                co,
                            ))
                        }),
                    };

                    // Enqueue the generator and hand control back to the outer loop.
                    self.frames.push(gen_frame);
                    return Ok(());
                }

                OpCode::OpAssertFail => {
                    return Err(frame.error(ErrorKind::AssertionFailed));
                }

                OpCode::OpCall => {
                    panic!("I think this branch is not live");
                }

                OpCode::OpTailCall => {
                    let callable = self.stack_pop();
                    self.tail_call_value(frame, callable)?;

                    // exit this loop and let the outer loop enter the new call
                    return Ok(());
                }

                OpCode::OpGetUpvalue(upv_idx) => {
                    let value = frame.upvalue(upv_idx).clone();
                    self.stack.push(value);
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
                        let value = self.stack_pop();
                        let trampoline = Thunk::force_trampoline(self, value).with_span(&frame)?;

                        // The current frame has to be re-enqueued to continue
                        // execution when the outer loop is finished with the
                        // trampoline.
                        let op_span = frame.current_light_span();
                        self.push_call_frame(span, frame);
                        self.push_trampoline(op_span, trampoline);

                        // Exit the loop at this point. The VM will resume code
                        // execution if the trampoline did not demand any further
                        // action.
                        return Ok(());
                    }
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
            .with_span(&frame)?;

        self.stack.push(Value::attrs(attrs));
        Ok(())
    }

    /// Access the I/O handle used for filesystem access in this VM.
    pub(crate) fn io(&self) -> &dyn EvalIO {
        &*self.io_handle
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
    pub fn emit_warning(&mut self, kind: WarningKind) {
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
        todo!()
    }

    /// Construct an error from the given ErrorKind and the source
    /// span of the current instruction.
    pub fn error(&self, kind: ErrorKind) -> Error {
        Error::new(kind, todo!())
    }

    fn call_builtin(&mut self, parent: CallFrame, builtin: Builtin) -> EvalResult<()> {
        let builtin_name = builtin.name();
        self.observer.observe_enter_builtin(builtin_name);

        let arg = self.stack_pop();
        let result = builtin.apply(self, arg).with_span(&parent)?;

        self.observer
            .observe_exit_builtin(builtin_name, &self.stack);

        self.stack.push(result);

        Ok(())
    }

    fn tail_call_value(&mut self, parent: CallFrame, callable: Value) -> EvalResult<()> {
        match callable {
            Value::Builtin(builtin) => self.call_builtin(parent, builtin),
            Value::Thunk(thunk) => self.tail_call_value(parent, thunk.value().clone()),

            Value::Closure(closure) => {
                let lambda = closure.lambda();
                self.observer.observe_tail_call(self.frames.len(), &lambda);

                self.push_call_frame(
                    parent.current_light_span(),
                    CallFrame {
                        lambda,
                        upvalues: closure.upvalues(),
                        ip: CodeIdx(0),
                        stack_offset: parent.stack_offset,
                        continuation: None,
                    },
                );

                Ok(())
            }

            // Attribute sets with a __functor attribute are callable.
            // TODO
            Value::Attrs(ref attrs) => match attrs.select("__functor") {
                None => Err(self.error(ErrorKind::NotCallable(callable.type_of()))),
                Some(_functor) => {
                    // if let Value::Thunk(thunk) = &functor {
                    //     fallible!(self, thunk.force(todo!()));
                    // }

                    // // The functor receives the set itself as its first argument
                    // // and needs to be called with it. However, this call is
                    // // synthetic (i.e. there is no corresponding OpCall for the
                    // // first call in the bytecode.)
                    // self.push(callable.clone());
                    // self.call_value(functor)?;
                    // let primed = self.pop();
                    // self.tail_call_value(primed)
                    todo!("how to do this?")
                }
            },
            _ => Err(parent.error(ErrorKind::NotCallable(callable.type_of()))),
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

    #[track_caller]
    pub fn call_with<I>(&mut self, callable: &Value, args: I) -> EvalResult<Value>
    where
        I: IntoIterator<Item = Value>,
        I::IntoIter: DoubleEndedIterator,
    {
        todo!("VM::call_with")
    }
}

/// Representation of a VM continuation; see:
/// https://en.wikipedia.org/wiki/Continuation-passing_style#CPS_in_Haskell
type Continuation = Box<dyn FnOnce(&mut VM) -> EvalResult<Trampoline>>;

/// A description of how to continue evaluation of a thunk when returned to by
/// the VM
///
/// This struct is used when forcing thunks to avoid stack-based recursion,
/// which for deeply nested evaluation can easily overflow the stack.
#[must_use = "this `Trampoline` may be a continuation request, which should be handled"]
#[derive(Default)]
pub struct Trampoline {
    /// The action to perform upon return to the trampoline
    pub action: Option<TrampolineAction>,

    /// The continuation to execute after the action has completed
    pub continuation: Option<Continuation>,
}

impl Trampoline {
    /// Add the execution of a new [`Continuation`] to the existing continuation
    /// of this `Trampoline`, returning the resulting `Trampoline`.
    pub fn append_to_continuation(self, f: Continuation) -> Self {
        Trampoline {
            action: self.action,
            continuation: match self.continuation {
                None => Some(f),
                Some(f0) => Some(Box::new(move |vm| {
                    let trampoline = f0(vm)?;
                    Ok(trampoline.append_to_continuation(f))
                })),
            },
        }
    }
}

/// Description of an action to perform upon return to a [`Trampoline`] by the VM
pub enum TrampolineAction {
    /// Enter a new stack frame
    EnterFrame {
        lambda: Rc<Lambda>,
        upvalues: Rc<Upvalues>,
        light_span: LightSpan,
        arg_count: usize,
    },

    /// Deep-force the value at the top of the stack.
    DeepForce,
}

pub struct OldVM<'o> {
    /// The VM call stack.  One element is pushed onto this stack
    /// each time a function is called or a thunk is forced.
    frames: Vec<CallFrame>,

    /// The VM value stack.  This is actually a "stack of stacks",
    /// with one stack-of-Values for each CallFrame in frames.  This
    /// is represented as a Vec<Value> rather than as
    /// Vec<Vec<Value>> or a Vec<Value> inside CallFrame for
    /// efficiency reasons: it avoids having to allocate a Vec on
    /// the heap each time a CallFrame is entered.
    stack: Vec<Value>,

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
}

/// The result of a VM's runtime evaluation.
pub struct RuntimeResult {
    pub value: Value,
    pub warnings: Vec<EvalWarning>,
}

impl<'o> OldVM<'o> {
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

    /// Returns the information needed to calculate the current span,
    /// but without performing that calculation.
    pub(crate) fn current_light_span(&self) -> LightSpan {
        LightSpan::new_delayed(self.frame().lambda.clone(), self.frame().ip - 1)
    }

    /// Access the I/O handle used for filesystem access in this VM.
    pub(crate) fn io(&self) -> &dyn EvalIO {
        &*self.io_handle
    }

    /// Construct an error from the given ErrorKind and the source
    /// span of the current instruction.
    pub fn error(&self, kind: ErrorKind) -> Error {
        Error::new(kind, self.current_span())
    }

    /// Push an already constructed warning.
    pub fn push_warning(&mut self, warning: EvalWarning) {
        self.warnings.push(warning);
    }

    /// Execute the given value in this VM's context, if it is a
    /// callable.
    ///
    /// The stack of the VM must be prepared with all required
    /// arguments before calling this and the value must have already
    /// been forced.
    pub fn call_value(&mut self, callable: &Value) -> EvalResult<()> {
        match callable {
            Value::Closure(c) => self.enter_frame(c.lambda(), c.upvalues(), 1),

            Value::Builtin(b) => todo!(), // self.call_builtin(b.clone()),

            Value::Thunk(t) => {
                debug_assert!(t.is_evaluated(), "call_value called with unevaluated thunk");
                self.call_value(&t.value())
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
        I::IntoIter: DoubleEndedIterator,
    {
        let mut num_args = 0_usize;
        for arg in args.into_iter().rev() {
            num_args += 1;
            self.push(arg);
        }

        if num_args == 0 {
            panic!("call_with called with an empty list of args");
        }

        self.call_value(callable)?;
        let mut res = self.pop();

        for _ in 0..(num_args - 1) {
            res.force(todo!()).map_err(|e| self.error(e))?;
            self.call_value(&res)?;
            res = self.pop();
        }

        Ok(res)
    }

    fn tail_call_value(&mut self, callable: Value) -> EvalResult<()> {
        match callable {
            Value::Builtin(builtin) => todo!(), // self.call_builtin(builtin),
            Value::Thunk(thunk) => self.tail_call_value(thunk.value().clone()),

            Value::Closure(closure) => {
                let lambda = closure.lambda();
                self.observer.observe_tail_call(self.frames.len(), &lambda);

                // Replace the current call frames internals with
                // that of the tail-called closure.
                let mut frame = self.frame_mut();
                frame.lambda = lambda;
                frame.upvalues = closure.upvalues();
                frame.ip = CodeIdx(0); // reset instruction pointer to beginning
                Ok(())
            }

            // Attribute sets with a __functor attribute are callable.
            Value::Attrs(ref attrs) => match attrs.select("__functor") {
                None => Err(self.error(ErrorKind::NotCallable(callable.type_of()))),
                Some(functor) => {
                    if let Value::Thunk(thunk) = &functor {
                        todo!("fallible!(self, thunk.force(todo!()))");
                    }

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

    /// Execute the given lambda in this VM's context, leaving the
    /// computed value on its stack after the frame completes.
    pub fn enter_frame(
        &mut self,
        lambda: Rc<Lambda>,
        upvalues: Rc<Upvalues>,
        arg_count: usize,
    ) -> EvalResult<()> {
        self.observer
            .observe_enter_frame(arg_count, &lambda, self.frames.len() + 1);

        let frame = CallFrame {
            lambda,
            upvalues,
            ip: CodeIdx(0),
            stack_offset: self.stack.len() - arg_count,
            continuation: None,
        };

        let starting_frames_depth = self.frames.len();
        self.frames.push(frame);

        // let result = loop {
        //     let op = self.inc_ip();

        //     self.observer
        //         .observe_execute_op(self.frame().ip, &op, &self.stack);

        //     let res = todo!();

        //     let mut retrampoline: Option<Continuation> = None;

        //     // we need to pop the frame before checking `res` for an
        //     // error in order to implement `tryEval` correctly.
        //     if self.frame().ip.0 == self.chunk().code.len() {
        //         let frame = self.frames.pop();
        //         retrampoline = frame.and_then(|frame| frame.continuation);
        //     }
        //     self.trampoline_loop(res?, retrampoline)?;
        //     if self.frames.len() == starting_frames_depth {
        //         break Ok(());
        //     }
        // };

        self.observer
            .observe_exit_frame(self.frames.len() + 1, &self.stack);

        todo!() // result
    }

    fn trampoline_loop(
        &mut self,
        mut trampoline: Trampoline,
        mut retrampoline: Option<Continuation>,
    ) -> EvalResult<()> {
        loop {
            if let Some(TrampolineAction::EnterFrame {
                lambda,
                upvalues,
                arg_count,
                light_span: _,
            }) = trampoline.action
            {
                let frame = CallFrame {
                    lambda,
                    upvalues,
                    ip: CodeIdx(0),
                    stack_offset: self.stack.len() - arg_count,
                    continuation: match retrampoline {
                        None => trampoline.continuation,
                        Some(retrampoline) => match trampoline.continuation {
                            None => None,
                            Some(cont) => Some(Box::new(|vm| {
                                Ok(cont(todo!())?.append_to_continuation(retrampoline))
                            })),
                        },
                    },
                };
                self.frames.push(frame);
                break;
            }

            match trampoline.continuation {
                None => {
                    if let Some(cont) = retrampoline.take() {
                        trampoline = cont(todo!())?;
                    } else {
                        break;
                    }
                }
                Some(cont) => {
                    trampoline = cont(todo!())?;
                    continue;
                }
            }
        }
        Ok(())
    }
}

pub fn run_lambda(
    nix_search_path: NixSearchPath,
    io_handle: Box<dyn EvalIO>,
    observer: &mut dyn RuntimeObserver,
    globals: Rc<GlobalsMap>,
    lambda: Rc<Lambda>,
) -> EvalResult<RuntimeResult> {
    let mut vm = VM::new(nix_search_path, io_handle, observer, globals);

    // Retain the top-level span of the expression in this lambda, as
    // synthetic "calls" in deep_force will otherwise not have a span
    // to fall back to.
    //
    // We exploit the fact that the compiler emits a final instruction
    // with the span of the entire file for top-level expressions.
    let root_span = lambda.chunk.get_span(CodeIdx(lambda.chunk.code.len() - 1));

    // Synthesise a frame that will instruct the VM to deep-force the final
    // value before returning it.
    vm.frames.push(Frame::Trampoline {
        span: root_span.into(),
        action: Some(TrampolineAction::DeepForce),
        continuation: None,
    });

    vm.frames.push(Frame::CallFrame {
        span: root_span.into(),
        call_frame: CallFrame {
            lambda,
            upvalues: Rc::new(Upvalues::with_capacity(0)),
            ip: CodeIdx(0),
            stack_offset: 0,
            continuation: None,
        },
    });

    vm.execute()
}
