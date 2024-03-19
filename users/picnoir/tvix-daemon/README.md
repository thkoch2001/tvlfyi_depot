# Tvix-daemon

A **super** incomplete implementation of a Nix-compatible daemon. Same as the original except it's backed by Tvix-Store.

For now, this is mostly used as a playground to implement the Nix daemon wire format in nix-compat.

On the long run, I hope this to be useful to get some real-world usage experience of tvix-store.

## Build

```sh
mg shell //tvix:shell
cargo build
```
