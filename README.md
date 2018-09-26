Finito
======

This is a Rust port of the Haskell state-machine library Finito. It is
slightly less featureful because it loses the ability to ensure that
side-effects are contained and because of a slight reduction in
expressivity, which makes it a bit more restrictive.

However, it still implements the FSM model well enough.

# Components

Finito is split up into multiple independent components (note: not all
of these exist yet), separating functionality related to FSM
persistence from other things.

* `finito`: Core abstraction implemented by Finito
* `finito-door`: Example implementation of a simple, lockable door
* `finito-postgres`: Persistent state-machines using Postgres

**Note**: The `finito` core library does not contain any tests. Its
coverage is instead provided by the `finito-door` library, which
actually implements an example FSM.

These are split out because the documentation for `finito-door` is
interesting regardless and because other Finito packages also need an
example implementation.
