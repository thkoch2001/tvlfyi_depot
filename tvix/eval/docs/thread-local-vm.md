# Thread-local storage for tvix::eval::VM

## The problem

`Value::force()` takes a `&mut VM` argument, since forcing a value requires
executing opcodes.  This means that `Value::nix_eq()` too must take a `&mut VM`,
since any sensible definition of equality will have to force thunks.

Unfortunately Rust's `PartialEq::eq()` function does not accept any additional
arguments like this, so `Value` cannot implement `PartialEq`.  Worse, structs
which *contain* `Value`s can't implement `PartialEq` either.

There are other situations like this that don't involve `PartialEq`, but it's
the most glaring one.  The main problem is that you need a `VM` in order to
force thunks, and thunks can be anywhere in a `Value`.

## Solving the problem with thread-locals

We could avoid threading the `&mut VM` through the entire codebase
by making it a thread-local.

To do this without a performance hit, we need to use LLVM
thread-locals, which are near zero-cost (basically just a memory
fence).  Unfortunately `#[thread_local]` [is
unstable][thread-local-unstable] and [unsafe in
general][thread-local-unsafe] for most of the cases where we would
want to use it.  There is one [exception][tls-const-init], however:
if a `!thread_local()` has a `const` initializer, the compiler will
insert a `#[thread_local]`; this special case is both safe and
stable.

The difficult decision is what the type of the thread-local should
be.  Since you can't get a mutable reference to a `thread_local!()`
it will have to be some interior-mutability-bestowing wrapper around
our current `struct VM`.  Here are the choices:

### `RefCell<VM>`

This is the obvious first choice, since it lets you borrow a
`RefMut<Target=VM>`.  The problem here is that we want to keep the
codebase written such that all the functions in `impl VM` still take
a `&mut self`.  This means that there will be an active mutable
borrow for the duration of `VM::call_builtin()`.  So if we implement
`PartialEq` by having `eq()` attempt a second mutable borrow from
the thread-local storage, it will fail since there is already an
active borrow.

The problem here is that you can't "unborrow" a `RefMut`.  There's no way around this.

#### Uglification

The only solution here is to rewrite all the functions in `impl VM`
so they don't take any kind of `self` argument, and then have them
do a short-lived `.borrow_mut()` from the thread-local `RefCell`
each time they want to modify one of the fields of `VM` (currently
`frames`, `stack`, `with_stack`, `warnings`).  Unfortunately this
has a fairly huge performance hit, because every single modification
to any part of `VM` will require a reference count
increment/decrement, and a conditional branch based on the check
(which will never fail) that the `RefCell` isn't already mutably
borrowed.  It will also impede a lot of rustc's optimizations.

### `Cell<VM>`

This is a non-starter because it means that in order to mutate any
field of `VM`, you have to move the entire `struct VM` out of the
`Cell`, mutate it, and move it back in.

### `Cell<Box<VM>>`

Now we're getting warmer.  This is the only solution worth
attempting to implement.  Here, we can move the `Box<VM>` out of the
cell with a single pointer-sized memory access.

We don't want to do the "uglification" described in the previous
section.  We are very fortunate that, sometime in mid-2019, the Rust
dieties [decreed by fiat][fiat-decree] that `&Cell<T>` and `&mut T`
are bit-for-bit identical, and even gave us mortals [safe][from_mut]
[wrappers][get_mut] for `mem::transmute()` in this case.

So now, we can have a `VM` method which takes `&mut self` call out
to some external code (like a builtin) and instead of passing the
`&mut self` to the external code, it can re-borrow the `&mut self`,
pass that re-borrow to `Cell::from_mut()`, then `Cell::swap()` it
into the thread-local storage cell for the duration of the external
code and `Cell::swap()` it back afterwards.  This whole dance gets
wrapped in a lexical block, and the borrow checker sees that the
`&Cell<Box<VM>>` returned by `Cell::from_mut()` lives only until the
end of the lexical block, so we get the `&mut self` back after the
close-brace for that block: NLL FTW.  This sounds like a lot of
work, but it compiles down to two pointer-sized loads, two
pointer-sized stores, and a memory fence, and it is incurred
basically only for `OpBuiltin`.

This all works, with only two issues:

1. `vm.rs` needs to be very careful to do the thread-local cell swap
   dance before calling anything that might call `PartialEq::eq()`
   (or any other method that expects to be able to pull the `VM` out
   of thread-local storage).  There is no compile-time check that we
   did the dance in all the right places.  If we forget to do the
   dance somewhere we'll get a runtime panic from `Option::expect()`
   (see next section).

2. Since we need to call `Cell::from_mut()` on a `Box<VM>` rather
   than a bare `VM`, we still need to rewrite all of `vm.rs` so that
   every function takes a `&mut Box<VM>` instead of a `&mut self`.
   This creates a huge amount of "noise" in the code.

Fortunately, it turns out that nearly all the "noise" that arises
from the second point can be eliminated by taking advantage of
[deref coercions][deref-coercions]!  This was the last "shoe to
drop".

There is still the issue of having to be careful about calls from
`vm.rs` to things outside that file, but it's manageable.

### `Cell<Option<Box<VM>>>`

In order to get the "safe and stable `#[thread_local]`" exception we
need a `const` initializer, which means we need to be able to put
something into the `Cell` that isn't a `VM`.  So the type needs to
be `Cell<Option<Box<VM>>>`.

Recall that you can't turn an `Option<&T>` into an `&Option<T>`.
The latter type has the "is this a `Some` or `None`" bit immediately
adjacent to the bits representing `T`.  So if I hand you a `t:&T`
and you wrap it as `Some(t)`, those bits aren't adjacent in memory.
This means that all the VM methods need to operate on an
`Option<Box<VM>>`.  Fortunately deref coercions eliminate the extra
code noise here too.

Note that Rust is clever and can find some sequence of bits that
aren't a valid `T`, so `sizeof(Option<T>)==sizeof(T)`.  And in fact,
`Box<T>` is one of these cases (and this is guaranteed).

# Closing thoughts, language-level support

This would have been easier with language-level support.

## What wouldn't help

Although it [it was decreed][fiat-decree] that `Cell<T>` and `&mut T` are
interchangeable, a `LocalKey<Cell<T>>` isn't quite the same
thing as a `Cell<T>`, so it isn't safe to have something like this:

```
impl<T> LocalKey<Cell<T>> {
  fn get_mut(&self) -> &mut T {
    ...
```

The problem here is that you can call `LocalKey<Cell<T>>::get_mut()` twice and
end up with two `&mut T`s that point to the same thing (mutable aliasing) which
results in undefined behavior.

## What would help

The ideal solution is for Rust to let you call arbitrary methods
`T::foo(&mut self...)` on a `LocalKey<Cell<T>>`.  This way you can
have one (and only one) `&mut T` at any syntactical point in your
program -- the `&mut self`.


[tls-const-init]: https://github.com/rust-lang/rust/pull/90774
[thread-local-unstable]: https://github.com/rust-lang/rust/issues/29594
[thread-local-unsafe-generally]: https://github.com/rust-lang/rust/issues/54366
[fiat-decree]: https://github.com/rust-lang/rust/issues/43038
[from_mut]: https://doc.rust-lang.org/stable/std/cell/struct.Cell.html#method.from_mut
[get_mut]: https://doc.rust-lang.org/stable/std/cell/struct.Cell.html#method.get_mut
[thread-local-unsafe]: [https://github.com/rust-lang/rust/issues/54366]
[deref-coercions]: https://doc.rust-lang.org/book/ch15-02-deref.html#implicit-deref-coercions-with-functions-and-methods
