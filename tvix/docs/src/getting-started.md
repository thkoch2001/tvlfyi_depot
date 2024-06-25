Getting Started
===============================================================================

Getting the code, a developer shell, & building the CLI
-------------------------------------------------------------------------------

Tvix can be built with the Rust standard `cargo build`. A Nix shell is provided
with the correctly-versioned tooling to build. [Direnv][] is also supported
from the monorepo to enable `mg` (TODO: what *is* `mg`?).

### TVL monorepo

```console
$ git clone https://code.tvl.fyi/depot.git
$ cd depot
$ direnv allow
$ mg shell //tvix:shell
$ cd tvix
$ cargo build
```

### Or just Tvix

```console
$ git clone https://code.tvl.fyi/depot.git:workspace=views/tvix.git
$ cd tvix
$ nix-shell
$ cargo build
```


Builds & tests
-------------------------------------------------------------------------------

All projects are built using [Nix][] to avoid ‘build pollution’ via the user’s
local environment.

If you have Nix installed and are contributing to a project tracked in this
repository, you can usually build the project by calling `nix-build -A
path.to.project`.

For example, to build a project located at `//tools/foo` you would call
`nix-build -A tools.foo`

If the project has tests, check that they still work before submitting your
change.

```admonish tip
You can use `git add --intent-to-add …` for adding new files to be picked up
by Git+Nix but not accidentally commit a file that isn’t ready.
```


[Direnv]: https://direnv.net
[Nix]: https://nixos.org/nix/
