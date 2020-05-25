Nix, or rather tazjin's fork thereof
------------------------------------

Nix is a new take on package management that is fairly unique. Because
of its purity aspects, a lot of issues found in traditional package
managers don't appear with Nix.

To find out more about the tool, usage and installation instructions,
please read the manual, which is available on the Nix website at
<http://nixos.org/nix/manual>.

This repository is [tazjin](https://tazj.in)'s fork of Nix.

## Fork background

Nix is a fantastic project with over a decade of demonstrated
real-world usage, but also with quite a few problems.

First of all, the project consists of two main components: The Nix
package collection ("[nixpkgs][]") and the package manager itself.

The package collection is an enormous effort with hundreds of
thousands of commits, encoding expert knowledge about lots of
different software and ways of building and managing it. It is a very
valuable piece of software.

The package manager however is an old C++ project with severe code
quality issues, little to no documentation, no consistent style and no
unit test coverage.

Its codebase is larger than it needs to be (often due to custom
reimplementations of basic functionality) and is mostly ad-hoc
structured, making it difficult to correctly implement large-scale
improvements.

In addition, the upstream Nix project is diverging from the opinions
of some community members via the introduction of concepts such as Nix
flakes.

To counteract these things I have decided to fork Nix.

## Fork goals

The things listed here are explicitly in-scope for work on the fork.
This list is not exhaustive, and it is very likely that many other
smaller things will be discovered along the way.

### nixpkgs compatibility

This fork will maintain compatibility with nixpkgs as much as
possible. If at any point we do need to diverge, we will do it in a
way that is backwards compatible.

### Code quality improvements

Code quality encompasses several different issues.

One goal is to slowly bring the codebase in line with the [Google C++
style guide][google-style]. Apart from the trivial reformatting (which
is already done), this means slowly chipping away at incorrectly
structured type hierarchies, usage of exceptions, usage of raw
pointers, global mutability and so on.

Another goal is to reduce the amount of code in Nix by removing custom
reimplementations of basic functionality (such as string splitting or
reading files).

For functionality that is not part of the C++17 standard library,
[Abseil][] will be the primary external library used.

### Explicit RPC mechanisms

Nix currently uses homegrown mechanisms of interacting with other Nix
binaries, for example for remote builds or interaction between the CLI
and the Nix daemon.

This will be replaced with [gRPC][].

### New sandboxing mechanism

Nix implements its own sandboxing mechanism. This was probably the
correct decision at the time, but is not necessary anymore because
Linux containers have become massively popular and lots of new tooling
is now available.

The goal is to replace the custom sandboxing implementation with
pluggable [OCI runtimes][oci], which will make it possible to use
arbitrary container runtimes such as [gVisor][] or [systemd-nspawn][]

### Pluggable Nix store backends

The current Nix store implementation will be removed from Nix' core
and instead be refactored into a gRPC API that can be implemented by
different backends.

### Builds as graph reductions

A Nix derivation that should be instantiated describes a build graph.
This graph will become a first-class citizen, making it possible to
distribute different parts of the computation to different nodes.

Implementing this properly will also allow us to improve the
implementation of import-from-derivation by explicitly moving through
different graph reduction stages.

## Fork non-goals

To set expectations, there are some explicit non-goals, too.

* Merging these changes back into upstream is not a goal, and maybe
  not even feasible. The core work has not even started yet and just
  basic cleanup has already created a diff of over 40 000 lines.

  This would likely also turn into a political effort, which I have no
  interest in.

* Improved performance is not an (initial) goal. Nix performance is
  very unevenly distributed across the codebase (some things have seen
  a lot of ad-hoc optimisation, others are written like inefficient
  toy implementations) and we simply don't know what effect the
  cleanup will have.

  Once the codebase is in a better state we will be able to start
  optimising it again while retaining readability, but this is not a
  goal until a later point in time.

* Compatibility with new upstream features is not a goal. Specifically
  we do not want Nix flakes, but other changes upstream makes will be
  considered for inclusion.

* Support for non-Linux systems. Currently Nix support Mac OS and
  potentially other systems, but this support will be dropped.

  Once we have OCI-compatible sandboxes and a store protocol it will
  be possible to reintroduce these with less friction.

## Contributing to the fork

My repository's default [contribution guidelines][contributing] apply.

In addition, please make sure that submitted code builds and is
formatted with `clang-format`, using the configuration found in this
folder.

## License

Nix is released under the LGPL v2.1

This product includes software developed by the OpenSSL Project for
use in the [OpenSSL Toolkit](http://www.OpenSSL.org/).

[nixpkgs]: https://github.com/NixOS/nixpkgs
[google-style]: https://google.github.io/styleguide/cppguide.html
[Abseil]: https://abseil.io/
[gRPC]: https://grpc.io/
[oci]: https://www.opencontainers.org/
[gVisor]: https://gvisor.dev/
[systemd-nspawn]: https://www.freedesktop.org/software/systemd/man/systemd-nspawn.html
[contributing]: https://git.tazj.in/about/docs/CONTRIBUTING.md
