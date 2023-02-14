//! This module implements generator logic for the VM. Generators are functions
//! used during evaluation which can suspend their execution during their
//! control flow, and request that the VM do something.
//!
//! This is used to keep the VM's stack size constant even when evaluating
//! deeply nested recursive data structures.
//!
//! We implement generators using the [`genawaiter`] crate.

use super::*;
use genawaiter::rc::{Co, Gen};

// -- Implementation of generic generator logic.

/// States that a generator can be in while being driven by the VM.
pub(super) enum GeneratorState {
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
pub(super) enum GeneratorMessage {
    /// No-op message. Initially passed to the generator to get it going, but it
    /// is ignored.
    NoOp,

    /// Request that the VM forces this value. This message is first sent to the
    /// VM with the unforced value, then returned to the generator with the
    /// forced result.
    ForceValue(Value),

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
}

pub(super) type Generator = Gen<
    GeneratorMessage,
    GeneratorMessage,
    Pin<Box<dyn Future<Output = Result<Value, ErrorKind>>>>,
>;

/// Helper function to provide type annotations which are otherwise difficult to
/// infer.
pub(super) fn pin_generator(
    f: impl Future<Output = Result<Value, ErrorKind>> + 'static,
) -> Pin<Box<dyn Future<Output = Result<Value, ErrorKind>>>> {
    Box::pin(f)
}

impl<'o> VM<'o> {
    /// Run a generator frame until it yields to the outer control loop, or runs
    /// to completion.
    pub(super) fn run_generator(
        &mut self,
        span: LightSpan,
        state: GeneratorState,
        mut generator: Generator,
    ) -> EvalResult<()> {
        // Determine what to send to the generator based on its state.
        let message = match state {
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
                    match request {
                        // This should not happen.
                        // TODO: consider splitting request/response?
                        GeneratorMessage::NoOp => {
                            panic!("Tvix bug: generator yielded NoOp")
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

                            let trampoline = Thunk::force_trampoline(self, value)
                                .map_err(|kind| Error::new(kind, span.span()))?;

                            self.push_trampoline(span, trampoline);
                            break;
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
                            let trampoline = Thunk::force_trampoline(self, value)
                                .map_err(|kind| Error::new(kind, span.span()))?;

                            self.push_trampoline(span, trampoline);
                            break;
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
                            let trampoline = Thunk::force_trampoline(self, value)
                                .map_err(|kind| Error::new(kind, span.span()))?;

                            self.push_trampoline(span, trampoline);
                            break;
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
                                generator: Gen::new(|co| pin_generator(nix_eq(co, a, b, false))),
                            };

                            self.frames.push(gen_frame);
                            return Ok(());
                        }
                    }
                }

                // Generator has completed, and its result value should
                // be left on the stack.
                genawaiter::GeneratorState::Complete(result) => {
                    let value = result.map_err(|kind| Error::new(kind, span.span()))?;
                    self.stack.push(value);
                    break;
                }
            }
        }

        Ok(())
    }
}

type GenCo = Co<GeneratorMessage, GeneratorMessage>;

// -- Implementation of concrete generator use-cases.

/// Force any value and return the evaluated result from the VM.
async fn force_value(co: &GenCo, val: Value) -> Value {
    match co.yield_(GeneratorMessage::ForceValue(val)).await {
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
async fn compare_values(co: &GenCo, a: Value, b: Value) -> Result<bool, ErrorKind> {
    match co.yield_(GeneratorMessage::NixEquality((a, b))).await {
        GeneratorMessage::ForceValue(value) => value.as_bool(),
        _ => panic!("Tvix bug: VM responded with incorrect generator message"),
    }
}

pub(super) async fn neo_resolve_with(
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

/// Compare two Nix values for equality, forcing nested parts of the structure
/// as needed.
///
/// This comparison needs to be invoked for nested values (e.g. in lists and
/// attribute sets) as well, which is done by suspending and asking the VM to
/// perform the nested comparison.
///
/// The `top_level` parameter controls whether this invocation is the top-level
/// comparison, or a nested value comparison. See
/// `//tvix/docs/value-pointer-equality.md`
pub(super) async fn nix_eq(
    co: GenCo,
    a: Value,
    b: Value,
    top_level: bool,
) -> Result<Value, ErrorKind> {
    // TODO: remove?
    // // This bit gets set to `true` (if it isn't already) as soon
    // // as we start comparing the contents of two
    // // {lists,attrsets} -- but *not* the contents of two thunks.
    // // See tvix/docs/value-pointer-equality.md for details.
    // let mut allow_pointer_equality_on_functions_and_thunks =
    //     allow_top_level_pointer_equality_on_functions_and_thunks;

    let a = match a {
        Value::Thunk(ref thunk) => {
            // If both values are thunks, and thunk comparisons are allowed by
            // pointer, do that and move on.
            if !top_level {
                if let Value::Thunk(t1) = &b {
                    if t1.ptr_eq(&thunk) {
                        return Ok(Value::Bool(true));
                    }
                }
            }

            force_value(&co, a).await
        }

        _ => a,
    };

    let b = match b {
        Value::Thunk(_) => force_value(&co, b).await,
        _ => b,
    };

    debug_assert!(!matches!(a, Value::Thunk(_)));
    debug_assert!(!matches!(b, Value::Thunk(_)));

    let result = match (a, b) {
        // Trivial comparisons
        (Value::Null, Value::Null) => true,
        (Value::Bool(b1), Value::Bool(b2)) => b1 == b2,
        (Value::String(s1), Value::String(s2)) => s1 == s2,
        (Value::Path(p1), Value::Path(p2)) => p1 == p2,

        // Numerical comparisons (they work between float & int)
        (Value::Integer(i1), Value::Integer(i2)) => i1 == i2,
        (Value::Integer(i), Value::Float(f)) => i as f64 == f,
        (Value::Float(f1), Value::Float(f2)) => f1 == f2,
        (Value::Float(f), Value::Integer(i)) => i as f64 == f,

        // List comparisons
        (Value::List(l1), Value::List(l2)) => {
            if l1.ptr_eq(&l2) {
                return Ok(Value::Bool(true));
            }

            if l1.len() != l2.len() {
                return Ok(Value::Bool(false));
            }

            for (vi1, vi2) in l1.into_iter().zip(l2.into_iter()) {
                if !compare_values(&co, vi1, vi2).await? {
                    return Ok(Value::Bool(false));
                }
            }

            true
        }

        (_, Value::List(_)) | (Value::List(_), _) => false,

        // Attribute set comparisons
        (Value::Attrs(a1), Value::Attrs(a2)) => {
            if a1.ptr_eq(&a2) {
                return Ok(Value::Bool(true));
            }

            match (a1.select("type"), a2.select("type")) {
                (Some(v1), Some(v2)) => todo!("drv comparison"),
                _ => {}
            };

            if a1.len() != a2.len() {
                return Ok(Value::Bool(false));
            }

            let iter1 = a1.into_iter_sorted();
            let iter2 = a2.into_iter_sorted();

            for ((k1, v1), (k2, v2)) in iter1.zip(iter2) {
                if k1 != k2 {
                    return Ok(Value::Bool(false));
                }

                if !compare_values(&co, v1, v2).await? {
                    return Ok(Value::Bool(false));
                }
            }

            true
        }

        (Value::Attrs(_), _) | (_, Value::Attrs(_)) => false,

        (Value::Closure(c1), Value::Closure(c2)) if !top_level => Rc::ptr_eq(&c1, &c2),

        // Everything else is either incomparable (e.g. internal types) or
        // false.
        _ => false,
    };

    Ok(Value::Bool(result))
}
