---
title: "Specification of the Nix language"
numbersections: true
author:
- tazjin
email:
- tazjin@tvl.su
lang: en-GB
---

The Nix Language
================

WARNING: This document is a work in progress. Please keep an eye on
[`topic:nix-spec`](https://cl.tvl.fyi/q/topic:nix-spec) for ongoing
CLs.

Nix is a general-purpose, functional programming language which this
document aims to describe.

## Background

Nix designed and implemented as part of the [Nix package
manager](https://nixos.org/nix). It is primarily used for generating
so-called derivations, which are data structures describing package
builds and similar operations in the package manager.

The language has been described in the
[thesis](https://edolstra.github.io/pubs/phd-thesis.pdf) introducing
the package manager, but only on a high-level. At the time of this
writing, Nix is informally specified (via its only complete
implementation in the package manager) and there is no complete
overview over its - sometimes surprising - semantics.

The primary project written in Nix is
[nixpkgs](https://github.com/NixOS/nixpkgs/). Uncertainties in the
process of writing this specification are resolved by investigating
patterns in nixpkgs, which we consider canonical.

## Introduction to Nix

Nix is a general-purpose, partially lazy, functional programming
language which provides higher-order functions, type reflection, many
primitive data types such as integers, strings and floats, as well as
compound data structures such as lists and attribute sets.

Nix has syntactic sugar for operations which are common in its domain,
such as attribute sets, and also provides a wide range of built-in
functions which have organically accumulated over time.

Nix has a variety of legacy features that are not in practical use,
but are documented in sections of this specification for the sake of
completeness.

This document describes the syntax and abstract semantics of the Nix
language, it leaves out implementation details about how Nix can be
interpreted/compiled/analysed etc.

### Program structure

This section describes the semantic structure of Nix, and how it
relates to the rest of the specification.

Each Nix program is a single [*expression*](#expressions) denoting a
[*value*](#values) (commonly a [*function*](#functions)). Each value
has a [*type*](#types), however this type is not statically known.

Nix code is modularised through the use of the
[*import*](#builtins-import) built-in function. No separate module
system exists.

In addition to chapters describing the building blocks mentioned
above, this specificiation also describes the [*syntax*](#syntax), the
available [built-in functions](#builtins), [*error handling*](#errors)
and known [*deficiciencies*](#deficiciencies) in the language.
