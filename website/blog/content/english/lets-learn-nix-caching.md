---
title: "Lets Learn Nix Caching"
date: 2020-03-17T18:05:38Z
draft: true
---

## TL;DR

1. I use `NixOS/nixpkgs-channels` instead of `NixOS/nixpkgs` and avoid
   `nix-channel`.

## More information

- By default the Nix package manager uses cache.nixos.org as a binary cache.
- Visit status.nixos.org
- `git clone git@github.com:NixOS/nixpkgs-channels` instead of
  `NixOS/nixpkgs`. The former mirrors the latter and uses Git branches to track
  the published channels.

## What is a Nix channel

If you run...

```shell
$ git clone git@github.com:NixOS/nixpkgs ~/nixpkgs
$ export NIX_PATH="nixpkgs=$(realpath ~/nixpkgs)"
```

One benefit to cloning nixpkgs is that you can browse the source code on your
machine using tools like `git` and `emacs`. You can also experimentally patch
and test Nix code this way.

If any of the above appeals to you, clone `nixpkgs-channels` instead.

The Nix maintainers build and test the commits from `nixpkgs` using Hydra. Tests
include reproducibility tests, etc.

Various channels have different verification phases.

The cache at cache.nixos.org is populate the cache at cache.nixos.org.

You want to increase the likelihood that you are hitting this cache. For
example, `google-chrome` takes hours to build.

## What is a binary cache?

## What is Hydra (Nix CI)?

## What is Cachix?
