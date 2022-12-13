tvix-store API
==============

This document outlines the design of the API exposed by tvix-store, as
well as other implementations of this store protocol.

The store API has three main consumers:

1. The evaluator (or more correctly, the CLI/coordinator, in the Tvix
   case) communicates with the store to:

   * Upload files and folders (e.g. from `builtins.path`, or `src =
     ./path` Nix expressions).
   * Read files from the store where necessary (e.g. when `nixpkgs` is
     located in the store, or for IFD).
   * Persist build results in the store

2. Tvix clients (such as users that have Tvix installed, or, depending
   on perspective, builder environments) expect the store to
   "materialise" on disk to provide a directory layout with store
   paths.

3. Other stores may communicate with a store to substitute already
   built store paths, i.e. a store acts as a binary cache towards
   other stores.

The store API attempts to reuse parts of its API between these three
consumers by making similarities explicit in the protocol. This leads
to a protocol that is slightly more complex than a simple "file
upload/download"-system, but at significantly greater efficiency.

## Blobs

Fundamentally, a store can be represented as a flat list of blobs with
some metadata. Each file (and even some metadata nodes, such as
directories) can be addressed by a cryptographically secure hash of
its contents[^blake3].

For example, lets consider a directory layout like this, with some
imaginary hashes of file contents:

```
.
├── file-1.txt        hash: 5891b5b522d5df086d0ff0b110fb
└── nested
    └── file-2.txt    hash: abc6fd595fc079d3114d4b71a4d8
```

A hash for the *folder* `nested` can now be created by considering the
folder as a data structure,

```nix
{
  "file-2.txt" = "abc6fd595fc079d3114d4b71a4d8";
}
```

and then hashing a serialised form of that data structure.

The same thing works one layer up, if we consider the whole folder:

```nix
{
  "file-1.txt" = "5891b5b522d5df086d0ff0b110fb";

  # v-- hash of the serialised folder above
  "nested" = "22c88612fa222511b9f2ca2eb18d";
}
```

With some metadata mapping the roots of folders to store paths, we can
now represent an entire Nix store as a flat list of blobs

## API overview

TODO: Which APIs exist (high-level!), and for each of them, who calls
it when?

## Example flows

TODO: High-level (!) example flows of how each client speaks to these APIs.

[^blake3]: We use [blake3][] hashes for this purpose, as they compose
    in a way that is very similar to how the merkle-tree of a Nix
    derivation itself looks. They are also very fast.

[blake3]: https://github.com/BLAKE3-team/BLAKE3
