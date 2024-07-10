# Syndicated Nix Actor

An actor for interacting with the [Nix](https://nixos.org/) daemon via the [Syndicated Actor Model](https://syndicate-lang.org/).

See [protocol.prs](./protocol.prs) for the Syndicate protocol [schema](https://preserves.dev/preserves-schema.html).

For an example configuration see [test.pr](./test.pr).

## Expressions as dataspaces

The actor exposes on its initial capability a gatekeeper that resolves requests in the form `<nix-repo { import: …, lookupPath: [ … ], store: … } >`. The resolved entity responds to observations as if it were a dataspace by asserting back lazily evaluated values from the imported expression.

### Caveats
- Functions are not observable, unless the function can be satisfied with the empty attrset `{}` as an argument.
- An observation that stops at an attrset with an `outPath` yields the `outPath` and not the attrset. This prevents abritrary recursion into derivation dependencies.

### TODO
Realise store-paths from expressions using local and remote buils.

## Worker protocol

The was once an abstraction of the Nix worker socket that could intermediate between clients and the worker but that code has been removed, refer to git history for that.
