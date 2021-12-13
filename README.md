<<<<<<< HEAD   (464bbc feat(sterni/aoc/2021): day 9 solution)
depot
=====

[![Build status](https://badge.buildkite.com/016bff4b8ae2704a3bbbb0a250784e6692007c582983b6dea7.svg?branch=canon)](https://buildkite.com/tvl/depot)

This repository is the [monorepo][] for the community around [The
Virus Lounge][tvl], containing our personal tools and infrastructure.
Everything in here is built using [Nix][].

A large portion of the software here is very self-referential, meaning that it
exists to sustain the operation of the repository. This is the case because we
partially see this as [an experiment][] in tooling for monorepos.

# Highlights

## Services

* Source code is available primarily through Sourcegraph on
  [cs.tvl.fyi](https://cs.tvl.fyi), where it is searchable and even semantically
  indexed. A lower-tech view of the repository is also available via cgit on
  [code.tvl.fyi](https://code.tvl.fyi).

  The repository can be cloned using `git` from `https://cl.tvl.fyi/depot`.

* All code in the depot, with the exception of code that is checked in to
  individual `//users` folders, needs to be reviewed. We use Gerrit on
  [cl.tvl.fyi](https://cl.tvl.fyi) for this.

* Issues are tracked via our own issue tracker on
  [b.tvl.fyi](https://b.tvl.fyi). Its source code lives at
  [`//web/panettone/`][panettone].

* Smaller todo-list entries which do not warrant a separate issue are listed at
  [todo.tvl.fyi](https://todo.tvl.fyi).

* We use Buildkite for CI. Recent builds are listed on
  [tvl.fyi/builds](https://tvl.fyi/builds) and pipelines are configured
  dynamically via
  [`//ops/pipelines`](https://cs.tvl.fyi/depot/-/tree/ops/pipelines).

* A search service that makes TVL services available via textual
  shortcuts is available: [atward](https://at.tvl.fyi)

All services that we host are deployed on NixOS machines that we manage. Their
configuration is tracked in `//ops/{modules,machines}`.

## Nix

* [`//nix/readTree`](https://cs.tvl.fyi/depot/-/blob/nix/readTree/README.md)
  contains the Nix code which automatically registers projects in our Nix
  attribute hierarchy based on their in-tree location
* `//nix/yants` contains **Y**et **A**nother **N**ix **T**ype **S**ystem, which
  we use for a variety of things throughout the repository
* `//nix/buildGo` implements a Nix library that can build Go software in the
  style of Bazel's `rules_go`. Go programs in this repository are built using
  this library.
* `//nix/buildLisp` implements a Nix library that can build Common Lisp
  software. Currently only SBCL is supported. Lisp programs in this repository
  are built using this library.
* `//web/bubblegum` contains a CGI-based web framework written in Nix.
* `//nix/nint`: A shebang-compatible interpreter wrapper for Nix.
* `//tvix` contains initial work towards a modular architecture for Nix.
* `//third_party/nix` contains [our fork][tvix] of the Nix package manager.

We have a variety of other tools and libraries in the `//nix` folder which may
be of interest.

## Packages / Libraries

* `//net/alcoholic_jwt` contains an easy-to-use JWT-validation library for Rust
* `//net/crimp` contains a high-level HTTP client using cURL for Rust
* `//tools/emacs-pkgs` contains various useful Emacs libraries, for example:
  * `dottime.el` provides [dottime][] in the Emacs modeline
  * `nix-util.el` provides editing utilities for Nix files
  * `term-switcher.el` is an ivy-function for switching between vterm buffers
  * `tvl.el` provides helper functions for interacting with the TVL monorepo
* `//lisp/klatre` provides a grab-bag utility library for Common Lisp

## User packages

Contributors to the repository have user directories under
[`//users`](https://cs.tvl.fyi/depot@canon/-/tree/users), which can be used for
personal or experimental code that does not require review.

Some examples:

* `//users/tazjin/homepage` && `//users/tazjin/blog`: A Nix-based static site
  generator which generates the web page and Atom feed for
  [tazj.in](https://tazj.in)
* `//users/grfn/xanthous`: A (WIP) TUI RPG, written in Haskell.
* `//users/tazjin/emacs`: tazjin's Emacs & EXWM configuration
* `//users/tazjin/finito`: A persistent finite-state machine library for Rust.

# Licensing

Unless otherwise stated in a subdirectory, all code is licensed under the MIT
license. See [LICENSE](./LICENSE) for details.

# Contributing

If you'd like to contribute to any of the tools in here, please check out the
[contribution guidelines](./docs/CONTRIBUTING.md) and our [code of
conduct](./docs/CODE_OF_CONDUCT.md).

IRC users can find us in [`#tvl`][tvl-irc] on [hackint][], which is also
reachable [via XMPP][hackint-xmpp] at [`#tvl@irc.hackint.org`][tvl-xmpp] (sic!).

Hackint also provide a [web chat][tvl-webchat].

[monorepo]: https://en.wikipedia.org/wiki/Monorepo
[tvl]: https://tvl.fyi
[Nix]: https://nixos.org/nix
[an experiment]: https://tvl.fyi/monorepo-doc
[panettone]: https://cs.tvl.fyi/depot@canon/-/tree/web/panettone
[tvix]: https://cs.tvl.fyi/depot/-/blob/third_party/nix/README.md
[dottime]: https://dotti.me
[tvl-irc]: ircs://irc.hackint.org:6697/#tvl
[hackint]: https://hackint.org/
[hackint-xmpp]: https://hackint.org/transport/xmpp
[tvl-xmpp]: xmpp:#tvl@irc.hackint.org?join
[tvl-webchat]: https://webirc.hackint.org/#ircs://irc.hackint.org/#tvl
=======
# briefcase

[![Build status](https://badge.buildkite.com/aa0d413bfeedcafd8719f977eadd40e04d0b5334fc7f58e8ee.svg)](https://buildkite.com/wpcarros-infrastructure/post-receive)

Welcome to my monorepo: briefcase.

Herein you will find a variety of libraries, packages, and documents. Some of
this work in finished and other work is incomplete or just a sketch for a
future project.

Where applicable, I try to include `README.md` files in some of the
subdirectories to help orient both myself and any onlookers.

## Languages

To give you a general idea of the source code inside of this monorepo, here is
the latest output from `tokei --hidden --sort code .`:

```text
-------------------------------------------------------------------------------
 Language            Files        Lines         Code     Comments       Blanks
-------------------------------------------------------------------------------
 Emacs Lisp             81        22267        13847         5661         2759
 Python                177        10575         7930          885         1760
 Elm                    34         5345         4277          219          849
 Haskell                50         4263         3111          428          724
 Nix                    66         1581         1379           66          136
 TypeScript             19         1345         1067           90          188
 Go                     17         1256          926          173          157
 Vim Script              2          766          470           87          209
 Elixir                 13          358          301            8           49
 JavaScript              9           77           73            0            4
 Lisp                    3           83           43           23           17
 Shell                   3           55           30           11           14
 Clojure                 2           10            8            0            2
 C                       1            6            5            0            1
 Rust                    1            5            3            1            1
-------------------------------------------------------------------------------
 Total                 478        47992        33470         7652         6870
-------------------------------------------------------------------------------
```

## Sign posts

Below I have outlined a few projects that you might find interesting. I am
using `//` to indicate the root of my monorepo, the directory in which this
`README.md` resides.

- `//boilerplate`: scaffolding for projects. Boilerplate's goal is to
  reduce the startup costs of a project.
- `//configs`: my dotfiles (e.g. `config.fish`, `init.vim`).
- `//emacs`: Emacs is both my preferred text editor and my window manager; with
  tens of thousands of lines of Emacs Lisp, you can safely assume that this
  directory hosts a lot of libraries and packages.
- `//monzo_ynab`: `systemd` timer unit that imports my Monzo (i.e. a U.K.-based
  online bank) transactions into the personal finance tool YNAB (i.e.
  youneedabudget.com).
- `//nixos`: my declarative configuration for my NixOS machines. If you are
  unfamiliar with Nix, I recommend reading about the NixOS project.
- `//tools`: some scripts and projects that simplify my life.
- `//website`: everything required to build my website, wpcarro.dev.

## Notes to self

Here are a few reminders when setting up a new machine:

- Ensure `~/.password-store` exists.
- Run `export_gpg` from a computer with my gpg credentials. Run `import_gpg`
  from the new machine.
- Ensure the new machine can access my Github.
>>>>>>> BRANCH (6123e9 playbooks: add hip_opening_challenge)
