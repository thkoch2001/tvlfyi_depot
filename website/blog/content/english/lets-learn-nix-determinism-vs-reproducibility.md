---
title: "Lets Learn Nix: Reproducibility"
date: 2020-03-17T12:06:47Z
draft: true
---

I am dedicating this page to defining and disambiguating some terminology. I
think it is important to use these terms precisely, so it may be worthwhile to
memorize these definitions and ensure that you are clarifying the discourse
rather than muddying it.

## Terms

- repeatable build:
- reproducible build:
- deterministic build:
- pure function:
- impure function:
- idempotent function:

TODO(wpcarro): Consistently and deliberately use reproducible and
deterministic.

## Repeatable vs. Reproducible

Is NixOS reproducible? Visit [@grhmc][who-grhmc]'s website,
[r13y.com](https://r13y.com), to find out.

At the time of this writing, 1519 of 1568 (i.e. 96.9%) of the paths in the
`nixos.iso_minimal.x86_64-linux` installation image are reproducible.

## What hinders reproducibility?

Timestamps.

If package A encodes a timestamp into its build artifact, then we can
demonstrate that package A is *not reproducible* simply by building it at two
different times and doing a byte-for-byte comparison of the build artifacts.

## Does Nix protect developers against non-determinism

Yes. But not entirely. How?

## Deterministic Nix derivation

```nix
{ pkgs ? import <nixpkgs> {}, ... }:

with pkgs;

stdenv.mkDerivation {
  name = "reproducible";
  phases = [ "buildPhase" ];
  buildPhase = "echo reproducible >$out";
}
```

## Non-deterministic Nix derivation

We can introduce some non-determinism into our build using the `date` function.

```nix
# file: /tmp/test.nix
{ pkgs ? import <nixpkgs> {}, ... }:

with pkgs;

stdenv.mkDerivation {
  name = "non-reproducible";
  phases = [ "buildPhase" ];
  buildPhase = "date >$out";
}
```

Then run...

```shell
$ nix-build /tmp/test.nix
$ nix-build /tmp/test.nix --check --keep-failed
```

## How do you test reproducibility?

We can use `cmp` to compare files byte-for-byte. The following comparison should
fail:

```shell
$ echo foo >/tmp/a
$ echo bar >/tmp/b
$ cmp --silent /tmp/{a,b}
$ echo $?
```

And the following comparison should succeed:

```shell
$ echo hello >/tmp/a
$ echo hello >/tmp/b
$ cmp --silent /tmp/{a,b}
$ echo $?
```

## Reproducible vs. deterministic

Reproducible builds *are* deterministic builds and deterministic build

## Deterministic, Reproducible, Pure, Idempotent, oh my

- A pure function has no side-effects.

- An idempotent function can be executed more than once with the same arguments
  without altering the side-effects.

- A deterministic function ensures that

## Deterministic vs. Reproducible

I can check if a build is reproducible using [these tools][wtf-repro-tools].

[wtf-repro-tools]: https://reproducible-builds.org/tools/
[who-grhmc]: https://twitter.com/grhmc
