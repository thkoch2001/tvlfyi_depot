<<<<<<< HEAD   (ff10b7 chore(3p): Remove gerrit-queue folder in preparation for ven)
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
# gerrit-queue

This daemon automatically rebases and submits changesets from a Gerrit
instance, ensuring they still pass CI.

In a usual gerrit setup with a linear master history, different developers
await CI feedback on a rebased changeset, then one clicks submit, and
effectively makes everybody else rebase again. `gerrit-queue` is meant to
remove these races to master.

Developers can set the `Autosubmit` label to `+1` on all changesets in a series,
and if all preconditions on are met ("submittable" in gerrit speech, this
usually means passing CI and passing Code Review), `gerrit-queue` takes care of
rebasing and submitting it to master

## How it works
Gerrit only knows about Changesets (and some relations to other changesets),
but usually developers think in terms of multiple changesets.

### Fetching changesets
`gerrit-queue` fetches all changesets from gerrit, and tries to identify these
chains of changesets. We call them `Series`. All changesets need to have strict
parent/child relationships to be detected (so if only half of the stack gets
rebased by the Gerrit Web interface, these are considered individual series.

Series are sorted by the number of changesets in them. This ensures longer
series are merged faster, and less rebases are triggered. In the future, this
might be extended to other metrics.

### Submitting changesets
The submitqueue has a Trigger() function, which gets periodically executed.

It can keep a reference to one single serie across multiple runs. This is
necessary if it previously rebased one serie to current HEAD and needs to wait
some time until CI feedback is there. If it wouldn't keep that state, it would
pick another series (with +1 from CI) and trigger a rebase on that one, so
depending on CI run times and trigger intervals, if not keepig this information
it'd end up rebasing all unrebased changesets on the same HEAD, and then just
pick one, instead of waiting for the one to finish.

The Trigger() function first instructs the gerrit client to fetch changesets
and assemble series.
If there is a `wipSerie` from a previous run, we check if it can still be found
in the newly assembled list of series (it still needs to contain the same
number of series. Commit IDs may differ, because the code doesn't reassemble a
`wipSerie` after scheduling a rebase.
If the `wipSerie` could be refreshed, we update the pointer with the newly
assembled series. If we couldn't find it, we drop it.

Now, we enter the main for loop. The first half of the loop checks various
conditions of the current `wipSerie`, and if successful, does the submit
("Submit phase"), the second half will pick a suitable new `wipSerie`, and
potentially do a rebase ("Pick phase").

#### Submit phase
We check if there is an existing `wipSerie`. If there isn't, we immediately go to
the "pick" phase.

The `wipSerie` still needs to be rebased on `HEAD` (otherwise, the submit queue
advanced outside of gerrit), and should not fail CI (logical merge conflict) -
otherwise we discard it, and continue with the picking phase.

If the `wipSerie` still contains a changeset awaiting CI feedback, we `return`
from the `Trigger()` function (and go back to sleep).

If the changeset is "submittable" in gerrit speech, and has the necessary
submit queue tag set, we submit it.

#### Pick phase
The pick phase finds a new `wipSerie`. It'll first try to find one that already
is rebased on the current `HEAD` (so the loop can just continue, and the next
submit phase simply submit), and otherwise fall back to a not-yet-rebased
serie. Because the rebase mandates waiting for CI, the code `return`s the
`Trigger()` function, so it'll be called again after waiting some time.

## Compile and Run
```sh
go generate
GERRIT_PASSWORD=mypassword go run main.go --url https://gerrit.mydomain.com --username myuser --project myproject
```
>>>>>>> BRANCH (24f5a6 gerrit: Use a Gerrit label instead of hashtag for autosubmit)
