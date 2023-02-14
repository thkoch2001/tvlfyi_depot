//! This module implements generator logic for the VM. Generators are functions
//! used during evaluation which can suspend their execution during their
//! control flow, and request that the VM do something.
//!
//! This is used to keep the VM's stack size constant even when evaluating
//! deeply nested recursive data structures.
//!
//! We implement generators using the [`genawaiter`] crate.

use genawaiter::rc::Co;
pub use genawaiter::rc::Gen;
use smol_str::SmolStr;
use std::fmt::Display;

use crate::value::SharedThunkSet;
use crate::warnings::WarningKind;
use crate::FileType;
use crate::{AddContext, NixString};

use super::*;

// -- Implementation of generic generator logic.

/// States that a generator can be in while being driven by the VM.
pub(crate) enum GeneratorState {
    /// Normal execution of the generator.
    Running,

    /// Generator is awaiting the result of a forced value.
    AwaitingValue,
}

/// Values that can be sent back and forth between generators and the main VM
/// loop, instructing the VM to perform certain actions or return certain values
/// to the generator.
///
/// Unfortunately, many of these messages are specific to a particular use-case
/// as they essentially have to encapsulate all interaction between the VM and
/// generators.
pub enum GeneratorMessage {
    /// No-op message. Initially passed to the generator to get it going, but it
    /// is ignored.
    NoOp,

    /// Request that the VM forces this value. This message is first sent to the
    /// VM with the unforced value, then returned to the generator with the
    /// forced result.
    ForceValue(Value),

    /// Request that the VM deep-forces the value.
    DeepForceValue(Value, SharedThunkSet),

    /// Request the value at the given index from the VM's with-stack, in forced
    /// state.
    ///
    /// The value is returned in the `ForceValue` message.
    WithValue(usize),

    /// Request the value at the given index from the *captured* with-stack, in
    /// forced state.
    CapturedWithValue(usize),

    /// Request that the two values be compared for Nix equality. The result is
    /// returned in the `ForceValue` message.
    // TODO: this bloats message size, hmm.
    NixEquality((Value, Value)),

    /// Push the given value to the VM's stack. This is used to prepare the
    /// stack for requesting a function call from the VM.
    ///
    /// The VM does not respond to this request, so the next message received is
    /// a `NoOp`.
    StackPush(Value),

    /// Pop a value from the stack and return it to the generator.
    StackPop,

    /// Request that the VM coerces this value to a string.
    StringCoerce((Value, CoercionKind)),

    /// Request that the VM calls the given value, with arguments already
    /// prepared on the stack.
    Call(Value),

    /// Request a call frame entering the given lambda immediately. This can be
    /// used to force thunks.
    EnterLambda {
        lambda: Rc<Lambda>,
        upvalues: Rc<Upvalues>,
        light_span: LightSpan,
    },

    /// Emit a runtime warning through the VM. Receives a NoOp-response.
    EmitWarning(WarningKind),

    /// Request a lookup in the VM's import cache, which tracks the
    /// thunks yielded by previously imported files.
    ImportCacheLookup(PathBuf),

    /// Request that the VM imports the given path through its I/O interface.
    PathImport(PathBuf),

    /// Request that the VM reads the given path to a string.
    ReadToString(PathBuf),

    /// Request that the VM checks whether the given path exists.
    PathExists(PathBuf),

    /// Request that the VM reads the given path.
    ReadDir(PathBuf),

    /// VM response with the contents of a directory.
    Directory(Vec<(SmolStr, FileType)>),
}

/// Human-readable representation of a generator message, used by observers.
impl Display for GeneratorMessage {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            GeneratorMessage::NoOp => write!(f, "no_op"),
            GeneratorMessage::ForceValue(v) => write!(f, "force_value({})", v),
            GeneratorMessage::DeepForceValue(v, _) => write!(f, "deep_force_value({})", v),
            GeneratorMessage::WithValue(_) => write!(f, "with_value"),
            GeneratorMessage::CapturedWithValue(_) => write!(f, "captured_with_value"),
            GeneratorMessage::NixEquality((v1, v2)) => write!(f, "nix_eq({}, {})", v1, v2),
            GeneratorMessage::StackPush(v) => write!(f, "stack_push({})", v),
            GeneratorMessage::StackPop => write!(f, "stack_pop"),
            GeneratorMessage::StringCoerce((v, kind)) => match kind {
                CoercionKind::Weak => write!(f, "weak_string_coerce({})", v),
                CoercionKind::Strong => write!(f, "strong_string_coerce({})", v),
            },
            GeneratorMessage::Call(v) => write!(f, "call({})", v),
            GeneratorMessage::EnterLambda { lambda, .. } => {
                write!(f, "enter_lambda({:p})", *lambda)
            }
            GeneratorMessage::EmitWarning(_) => write!(f, "emit_warning"),
            GeneratorMessage::ImportCacheLookup(p) => {
                write!(f, "import_cache_lookup({})", p.to_string_lossy())
            }
            GeneratorMessage::PathImport(p) => write!(f, "path_import({})", p.to_string_lossy()),
            GeneratorMessage::ReadToString(p) => {
                write!(f, "read_to_string({})", p.to_string_lossy())
            }
            GeneratorMessage::PathExists(p) => write!(f, "path_exists({})", p.to_string_lossy()),
            GeneratorMessage::ReadDir(p) => write!(f, "read_dir({})", p.to_string_lossy()),
            GeneratorMessage::Directory(_) => todo!("split request/response"),
        }
    }
}

pub(crate) type Generator = Gen<
    GeneratorMessage,
    GeneratorMessage,
    Pin<Box<dyn Future<Output = Result<Value, ErrorKind>>>>,
>;

/// Helper function to provide type annotations which are otherwise difficult to
/// infer.
pub fn pin_generator(
    f: impl Future<Output = Result<Value, ErrorKind>> + 'static,
) -> Pin<Box<dyn Future<Output = Result<Value, ErrorKind>>>> {
    Box::pin(f)
}

impl<'o> VM<'o> {
    /// Run a generator frame until it yields to the outer control loop, or runs
    /// to completion.
    ///
    /// The return value indicates whether the generator has completed (true),
    /// or was suspended (false).
    pub(crate) fn run_generator(
        &mut self,
        span: LightSpan,
        state: GeneratorState,
        mut generator: Generator,
    ) -> EvalResult<bool> {
        // Determine what to send to the generator based on its state.
        let mut message = match state {
            GeneratorState::Running => GeneratorMessage::NoOp,

            // If control returned here, and the generator is
            // awaiting a value, send it the top of the stack.
            GeneratorState::AwaitingValue => GeneratorMessage::ForceValue(self.stack_pop()),
        };

        loop {
            match generator.resume_with(message) {
                // If the generator yields, it contains an instruction
                // for what the VM should do.
                genawaiter::GeneratorState::Yielded(request) => {
                    self.observer.observe_generator_request(&request);

                    match request {
                        // This should not happen.
                        // TODO: consider splitting request/response?
                        GeneratorMessage::NoOp => {
                            panic!("Tvix bug: generator yielded NoOp")
                        }

                        GeneratorMessage::StackPush(value) => {
                            self.stack.push(value);
                            message = GeneratorMessage::NoOp;
                        }

                        GeneratorMessage::StackPop => {
                            message = GeneratorMessage::ForceValue(self.stack_pop());
                        }

                        // Generator has requested a force, which means that
                        // this function prepares the frame stack and yields
                        // back to the outer VM loop.
                        GeneratorMessage::ForceValue(value) => {
                            self.frames.push(Frame::Generator {
                                generator,
                                span: span.clone(),
                                state: GeneratorState::AwaitingValue,
                            });

                            self.frames.push(Frame::Generator {
                                span,
                                state: GeneratorState::Running,
                                generator: Gen::new(|co| pin_generator(value.neo_force(co))),
                            });

                            return Ok(false);
                        }

                        // Generator has requested a deep-force.
                        GeneratorMessage::DeepForceValue(value, thunk_set) => {
                            // Re-enqueue self
                            self.frames.push(Frame::Generator {
                                generator,
                                span: span.clone(),
                                state: GeneratorState::AwaitingValue,
                            });

                            self.frames.push(Frame::Generator {
                                span,
                                state: GeneratorState::Running,
                                generator: Gen::new(|co| {
                                    pin_generator(value.deep_force(co, thunk_set))
                                }),
                            });

                            return Ok(false);
                        }

                        // Generator has requested a value from the with-stack.
                        // Logic is similar to `ForceValue`, except with the
                        // value being taken from that stack.
                        GeneratorMessage::WithValue(idx) => {
                            self.frames.push(Frame::Generator {
                                generator,
                                span: span.clone(),
                                state: GeneratorState::AwaitingValue,
                            });

                            let value = self.stack[self.with_stack[idx]].clone();
                            self.frames.push(Frame::Generator {
                                span,
                                state: GeneratorState::Running,
                                generator: Gen::new(|co| pin_generator(value.neo_force(co))),
                            });

                            return Ok(false);
                        }

                        // Generator has requested a value from the *captured*
                        // with-stack. Logic is same as above, except for the
                        // value being from that stack.
                        GeneratorMessage::CapturedWithValue(idx) => {
                            self.frames.push(Frame::Generator {
                                generator,
                                span: span.clone(),
                                state: GeneratorState::AwaitingValue,
                            });

                            let call_frame = self.last_call_frame()
                                .expect("Tvix bug: generator requested captured with-value, but there is no call frame");

                            let value = call_frame.upvalues.with_stack().unwrap()[idx].clone();
                            self.frames.push(Frame::Generator {
                                span,
                                state: GeneratorState::Running,
                                generator: Gen::new(|co| pin_generator(value.neo_force(co))),
                            });

                            return Ok(false);
                        }

                        GeneratorMessage::NixEquality((a, b)) => {
                            // Re-enqueue the *current* generator first, as this
                            // is a recursive comparison.
                            self.frames.push(Frame::Generator {
                                generator,
                                span: span.clone(),
                                state: GeneratorState::AwaitingValue,
                            });

                            let gen_frame = Frame::Generator {
                                span,
                                state: GeneratorState::Running,
                                generator: Gen::new(|co| pin_generator(a.nix_eq(b, co, false))),
                            };

                            self.frames.push(gen_frame);
                            return Ok(false);
                        }

                        GeneratorMessage::StringCoerce((val, kind)) => {
                            self.frames.push(Frame::Generator {
                                generator,
                                span: span.clone(),
                                state: GeneratorState::AwaitingValue,
                            });

                            self.frames.push(Frame::Generator {
                                span,
                                state: GeneratorState::Running,
                                generator: Gen::new(|co| {
                                    pin_generator(val.neo_coerce_to_string(co, kind))
                                }),
                            });

                            return Ok(false);
                        }

                        GeneratorMessage::Call(callable) => {
                            self.frames.push(Frame::Generator {
                                generator,
                                span: span.clone(),
                                state: GeneratorState::AwaitingValue,
                            });

                            self.tail_call_value(span, None, callable)?;

                            return Ok(false);
                        }

                        GeneratorMessage::EnterLambda {
                            lambda,
                            upvalues,
                            light_span,
                        } => {
                            self.frames.push(Frame::Generator {
                                generator,
                                span: span.clone(),
                                state: GeneratorState::AwaitingValue,
                            });

                            self.frames.push(Frame::CallFrame {
                                span: light_span,
                                call_frame: CallFrame {
                                    lambda,
                                    upvalues,
                                    ip: CodeIdx(0),
                                    stack_offset: self.stack.len(),
                                },
                            });

                            return Ok(false);
                        }

                        GeneratorMessage::EmitWarning(kind) => {
                            self.emit_warning(kind);
                            message = GeneratorMessage::NoOp;
                        }

                        GeneratorMessage::ImportCacheLookup(path) => {
                            if let Some(cached) = self.import_cache.get(&path) {
                                message = GeneratorMessage::ForceValue(cached.clone());
                            } else {
                                message = GeneratorMessage::NoOp;
                            }
                        }

                        GeneratorMessage::PathImport(path) => {
                            let imported = self
                                .io_handle
                                .import_path(&path)
                                .map_err(|kind| Error::new(kind, span.span()))?;

                            message = GeneratorMessage::PathImport(imported);
                        }

                        GeneratorMessage::ReadToString(path) => {
                            let content = self
                                .io_handle
                                .read_to_string(path)
                                .map_err(|kind| Error::new(kind, span.span()))?;

                            message = GeneratorMessage::ForceValue(Value::String(content.into()))
                        }

                        GeneratorMessage::PathExists(path) => {
                            let exists = self
                                .io_handle
                                .path_exists(path)
                                .map(Value::Bool)
                                .map_err(|kind| Error::new(kind, span.span()))?;

                            message = GeneratorMessage::ForceValue(exists);
                        }

                        GeneratorMessage::ReadDir(path) => {
                            let dir = self
                                .io_handle
                                .read_dir(path)
                                .map_err(|kind| Error::new(kind, span.span()))?;

                            message = GeneratorMessage::Directory(dir);
                        }

                        GeneratorMessage::Directory(_) => todo!("split request/response"),
                    }
                }

                // Generator has completed, and its result value should
                // be left on the stack.
                genawaiter::GeneratorState::Complete(result) => {
                    let value = result.map_err(|kind| Error::new(kind, span.span()))?;
                    self.stack.push(value);
                    return Ok(true);
                }
            }
        }
    }
}

pub type GenCo = Co<GeneratorMessage, GeneratorMessage>;

// -- Implementation of concrete generator use-cases.

/// Request that the VM place the given value on its stack.
pub async fn request_stack_push(co: &GenCo, val: Value) {
    match co.yield_(GeneratorMessage::StackPush(val)).await {
        GeneratorMessage::NoOp => {}
        _ => panic!("Tvix bug: VM responded with incorrect generator message"),
    }
}

/// Request that the VM pop a value from the stack and return it to the
/// generator.
pub async fn request_stack_pop(co: &GenCo) -> Value {
    match co.yield_(GeneratorMessage::StackPop).await {
        GeneratorMessage::ForceValue(value) => value,
        _ => panic!("Tvix bug: VM responded with incorrect generator message"),
    }
}

/// Force any value and return the evaluated result from the VM.
pub async fn request_force(co: &GenCo, val: Value) -> Value {
    if let Value::Thunk(_) = val {
        match co.yield_(GeneratorMessage::ForceValue(val)).await {
            GeneratorMessage::ForceValue(value) => value,
            _ => panic!("Tvix bug: VM responded with incorrect generator message"),
        }
    } else {
        val
    }
}

/// Call the given value as a callable. The argument(s) must already be prepared
/// on the stack.
pub async fn request_call(co: &GenCo, val: Value) -> Value {
    let val = request_force(co, val).await;
    match co.yield_(GeneratorMessage::Call(val)).await {
        GeneratorMessage::ForceValue(value) => value,
        _ => panic!("Tvix bug: VM responded with incorrect generator message"),
    }
}

/// Helper function to call the given value with the provided list of arguments.
/// This uses the StackPush and Call messages under the hood.
pub async fn request_call_with<I>(co: &GenCo, mut callable: Value, args: I) -> Value
where
    I: IntoIterator<Item = Value>,
    I::IntoIter: DoubleEndedIterator,
{
    let mut num_args = 0_usize;
    for arg in args.into_iter().rev() {
        num_args += 1;
        request_stack_push(co, arg).await;
    }

    debug_assert!(num_args > 0, "call_with called with an empty list of args");

    while num_args > 0 {
        callable = request_call(co, callable).await;
        num_args -= 1;
    }

    callable
}

pub async fn request_string_coerce(co: &GenCo, val: Value, kind: CoercionKind) -> NixString {
    match val {
        Value::String(s) => s,
        _ => match co.yield_(GeneratorMessage::StringCoerce((val, kind))).await {
            GeneratorMessage::ForceValue(value) => value
                .to_str()
                .expect("coerce_to_string always returns a string"),
            _ => panic!("Tvix bug: VM responded with incorrect generator message"),
        },
    }
}

/// Deep-force any value and return the evaluated result from the VM.
pub async fn request_deep_force(co: &GenCo, val: Value, thunk_set: SharedThunkSet) -> Value {
    match co
        .yield_(GeneratorMessage::DeepForceValue(val, thunk_set))
        .await
    {
        GeneratorMessage::ForceValue(value) => value,
        _ => panic!("Tvix bug: VM responded with incorrect generator message"),
    }
}

/// Fetch and force a value on the with-stack from the VM.
async fn fetch_forced_with(co: &GenCo, idx: usize) -> Value {
    match co.yield_(GeneratorMessage::WithValue(idx)).await {
        GeneratorMessage::ForceValue(value) => value,
        _ => panic!("Tvix bug: VM responded with incorrect generator message"),
    }
}

/// Fetch and force a value on the *captured* with-stack from the VM.
async fn fetch_captured_with(co: &GenCo, idx: usize) -> Value {
    match co.yield_(GeneratorMessage::CapturedWithValue(idx)).await {
        GeneratorMessage::ForceValue(value) => value,
        _ => panic!("Tvix bug: VM responded with incorrect generator message"),
    }
}

/// Ask the VM to compare two values for equality.
pub(crate) async fn check_equality(co: &GenCo, a: Value, b: Value) -> Result<bool, ErrorKind> {
    match co.yield_(GeneratorMessage::NixEquality((a, b))).await {
        GeneratorMessage::ForceValue(value) => value.as_bool(),
        _ => panic!("Tvix bug: VM responded with incorrect generator message"),
    }
}

/// Emit a runtime warning.
pub(crate) async fn emit_warning(co: &GenCo, kind: WarningKind) {
    match co.yield_(GeneratorMessage::EmitWarning(kind)).await {
        GeneratorMessage::NoOp => {}
        _ => panic!("Tvix bug: VM responded with incorrect generator message"),
    }
}

/// Request that the VM enter the given lambda.
pub(crate) async fn request_enter_lambda(
    co: &GenCo,
    lambda: Rc<Lambda>,
    upvalues: Rc<Upvalues>,
    light_span: LightSpan,
) -> Value {
    let msg = GeneratorMessage::EnterLambda {
        lambda,
        upvalues,
        light_span,
    };

    match co.yield_(msg).await {
        GeneratorMessage::ForceValue(value) => value,
        _ => panic!("Tvix bug: VM responded with incorrect generator message"),
    }
}

/// Request a lookup in the VM's import cache.
pub(crate) async fn request_import_cache_lookup(co: &GenCo, path: PathBuf) -> Option<Value> {
    match co.yield_(GeneratorMessage::ImportCacheLookup(path)).await {
        GeneratorMessage::ForceValue(value) => Some(value),
        GeneratorMessage::NoOp => None,
        _ => panic!("Tvix bug: VM responded with incorrect generator message"),
    }
}

/// Request that the VM import the given path.
pub(crate) async fn request_path_import(co: &GenCo, path: PathBuf) -> PathBuf {
    match co.yield_(GeneratorMessage::PathImport(path)).await {
        GeneratorMessage::PathImport(path) => path,
        _ => panic!("Tvix bug: VM responded with incorrect generator message"),
    }
}

pub(crate) async fn request_read_to_string(co: &GenCo, path: PathBuf) -> Value {
    match co.yield_(GeneratorMessage::ReadToString(path)).await {
        GeneratorMessage::ForceValue(value) => value,
        _ => panic!("Tvix bug: VM responded with incorrect generator message"),
    }
}

pub(crate) async fn request_path_exists(co: &GenCo, path: PathBuf) -> Value {
    match co.yield_(GeneratorMessage::PathExists(path)).await {
        GeneratorMessage::ForceValue(value) => value,
        _ => panic!("Tvix bug: VM responded with incorrect generator message"),
    }
}

pub(crate) async fn request_read_dir(co: &GenCo, path: PathBuf) -> Vec<(SmolStr, FileType)> {
    match co.yield_(GeneratorMessage::ReadDir(path)).await {
        GeneratorMessage::Directory(dir) => dir,
        _ => panic!("Tvix bug: VM responded with incorrect generator message"),
    }
}

pub(crate) async fn neo_resolve_with(
    ident: String,
    vm_with_len: usize,
    upvalue_with_len: usize,
    co: GenCo,
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

/// Call the given value as if it was an attribute set containing a functor. The
/// arguments must already be prepared on the stack when a generator frame from
/// this function is invoked.
///
pub(crate) async fn call_functor(co: GenCo, value: Value) -> Result<Value, ErrorKind> {
    let attrs = value.to_attrs()?;

    match attrs.select("__functor") {
        None => Err(ErrorKind::NotCallable("set without `__functor_` attribute")),
        Some(functor) => {
            // The functor receives the set itself as its first argument and
            // needs to be called with it.
            let functor = request_force(&co, functor.clone()).await;
            let primed = request_call_with(&co, functor, [value]).await;
            Ok(request_call(&co, primed).await)
        }
    }
}
