Getting Started
===============================================================================

Getting the code, a developer shell, & building the CLI
-------------------------------------------------------------------------------

Tvix can be built with the Rust standard `cargo build`. A Nix shell is provided
with the correctly-versioned tooling to build.

### TVL monorepo

```console
$ git clone https://code.tvl.fyi/depot.git
$ cd depot
$ nix-shell
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
