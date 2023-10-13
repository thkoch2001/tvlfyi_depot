---
author:
- Florian Klink
date: 2023-09-09
title: "tvix-store: A content-addressed file system and sync protocol"
theme: moon
revealOptions:
  transition: 'fade'
---

## tvix-store
### A content-addressed file system and sync protocol

2023-09-13

Florian Klink / flokli

---

## Whoami

- <!-- .element: class="fragment" -->
  flokli
- <!-- .element: class="fragment" -->
  Nix/NixOS contributor
  - maintain systemd, nss and more low-level stuff there
- <!-- .element: class="fragment" -->
  Freelance Nix/DevOps consultant

Note: more Kubernetes/DevOps exposure with work

---

## What is tvix-store?
- <!-- .element: class="fragment" -->
  A new implementation of a content-addressed "storage system"
  - <!-- .element: class="fragment" -->
    part of the Tvix Project, a (WIP) reimplementation of Nix and auxillary components in Rust
  - <!-- .element: class="fragment" -->
    Storage model: think about git trees and its Merkle DAG…
  - <!-- .element: class="fragment" -->
    … but with nicer wire format (`.proto`) and hash function (blake3)

---

## Storage model
- <!-- .element: class="fragment" -->
  Once you know the root: everything else is content-addressed
   - <!-- .element: class="fragment" -->
     No timestamps, no uid/gid, no xattrs, only one way to represent the same tree
- <!-- .element: class="fragment" -->
  Automatic dedup of identical subtrees in different file system trees
- <!-- .element: class="fragment" -->
  Automatic dedup of identical blobs (and you can do more chunking underneath too)

---

## Storage model (cont.)
- <!-- .element: class="fragment" -->
  Granular seekable access into blobs
- <!-- .element: class="fragment" -->
  verified streaming thanks to BLAKE3 and Bao, faulty data is detected early on
- <!-- .element: class="fragment" -->
  Everything below can be retrieved from anyone without having to trust (P2P substitution, CDNs, …)

---

## Usecases
- <!-- .element: class="fragment" -->
  File system tree delivery
- <!-- .element: class="fragment" -->
  Container image delivery
- <!-- .element: class="fragment" -->
  Backing store for VCS
- <!-- .element: class="fragment" -->
  Granular access into large datasets

---

## Status
- <!-- .element: class="fragment" -->
  In-memory backend, a local K/V backend (Sled)
- <!-- .element: class="fragment" -->
  FUSE filesystem
- <!-- .element: class="fragment" -->
  A gRPC API to transfer things, bindings for golang and rust
- <!-- .element: class="fragment" -->
  some object storage backends in development (GCS, NATS)
- <!-- .element: class="fragment" -->
  FUTUREWORK: more storage backends / store composition / in-kernel module?

Notes: of course you can use your own network protocol too, like HTTP CAS or iroh....plug different stores together to represent caches, blobfs

---

## Contributing

- <!-- .element: class="fragment" -->
  Join the IRC channel (`#tvl` on `hackint`), bridged to Matrix and XMPP
- <!-- .element: class="fragment" -->
  Check our issue tracker
- <!-- .element: class="fragment" -->
  Try to use it and tell us how you broke it!

Note: if this sounds useful to you, reach out!

---

# Thanks!

<style>
.container{
    display: flex;
}
.col{
    flex: 1;
}
</style>

<div class="container">

<div class="col">
Florian Klink / <a href="https://flokli.de">flokli.de</a><br />
<img src="qrcode-flokli.svg" />
</div>

<div class="col">
Tvix / <a href="https://tvix.dev">tvix.dev</a><br />
<img src="qrcode-tvix.svg" />
</div>

</div>

---

## Structure

[tvix-store graph](tvix-store-graph-blob-directory.svg)
