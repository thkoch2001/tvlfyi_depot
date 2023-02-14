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

// -- Implementation of concrete generator use-cases.

type GenCo = Co<GeneratorMessage, GeneratorMessage>;

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
