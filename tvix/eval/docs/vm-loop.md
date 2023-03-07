tvix-eval VM loop
=================

This document describes the new tvix-eval VM execution loop implemented in the
chain focusing around cl/8104.

## Background

The VM loop implemented in Tvix prior to cl/8104 had several functions:

1. Advancing the instruction pointer for a chunk of Tvix bytecode and
   executing instructions in a loop until a result was yielded.

2. Tracking Nix call frames as functions/thunks were entered/exited.

3. Catching trampoline requests returned from instructions to force suspended
   thunks without increasing stack size *where possible*.

4. Handling trampolines through an inner trampoline loop, switching between a
   code execution mode and execution of subsequent trampolines.

This implementation of the trampoline logic was added on to the existing VM,
which previously always recursed for thunk forcing. There are some cases (for
example values that need to be forced *inside* of the execution of a builtin)
where trampolines could not previously be used, and the VM recursed anyways.

As a result of this trampoline logic being added "on top" of the existing VM
loop the code became quite difficult to understand. This led to several bugs,
for example: b/251, b/246, b/245, and b/238.

These bugs were tricky to deal with, as we had to try and make the VM do
things that are somewhat difficult to fit into its model. We could of course
keep extending the trampoline logic to accommodate all sorts of concepts (such
as finalisers), but that seems like it does not solve the root problem.

## New VM loop

In cl/8104, a unified new solution is implemented with which the VM is capable
of evaluating everything without increasing the call stack size.

This is done by introducing a new frame stack in the VM, on which execution
frames are enqueued that are either:

1. A call frame, consisting of Tvix bytecode that evaluates compiled Nix code.
2. A generator frame, consisting of some VM logic implemented in pure Rust
   code that can be *suspended* when it hits a point where the VM would
   previously need to recurse.

We do this by making use of the `async` *keyword* in Rust, but notably
*without* introducing asynchronous I/O in tvix-eval (the complexity of which
is undesirable for us).

Specifically, when writing a Rust function that uses the `async` keyword, such
as:

```rust
fn some_builtin(input: Value) -> Result<Value, ErrorKind> {
  let mut out = NixList::new();

  for element in input.to_list()? {
    let result = do_something_that_requires_the_vm(element).await;
    out.push(result);
  }

  Ok(out)
}
```

The compiler actually generates a state-machine under-the-hood which allows
the execution of that function to be *suspended* whenever it hits an `await`.

We use the [`genawaiter`][] crate that gives us a data structure and simple
interface for getting instances of these state machines that can be stored in
a struct (in our case, a *generator frame*).

The execution of the VM then becomes the execution of an *outer loop*, which
is responsible for selecting the next generator frame to execute, and two
*inner loops*, which drive the execution of a call frame or generator frame
forward until it either yields a value or asks to be suspended in favour of
another frame.

All "communication" between frames happens solely through values left on the
stack: Whenever a frame of either type runs to completion, it is expected to
leave a *single* value on the stack. It follows that the whole VM, upon
completion of the last (or initial, depending on your perspective) frame
yields its result as the return value.

The core of the VM restructuring is cl/8104, unfortunately one of the largest
single commit changes we've had to make yet, as it touches pretty much all
areas of tvix-eval. The introduction of the generators and the
message/response system we built to request something from the VM, suspend a
generator, and wait for the return is in cl/8148.

The next sections describe in detail how the three different loops work.

### Outer VM loop

TODO! https://app.excalidraw.com/l/3CqeGp7p0yV/5e5Id7aSnOP

### Inner call frame loop

TODO!

### Inner generator loop

TODO!

## Advantages & Disadvantages of the approach

TODO!

## Alternatives considered

1. Tacking on more functionality onto the existing VM loop
   implementation to accomodate problems as they show up. This is not
   preferred as the code is already getting messy.

2. ... ?

[`genawaiter`]: https://docs.rs/genawaiter/
