# Syndicated Nix Actor

An actor for interacting with the [Nix](https://nixos.org/) daemon via the [Syndicated Actor Model](https://syndicate-lang.org/).

See [protocol.prs](./protocol.prs) for the Syndicate protocol [schema](https://preserves.dev/preserves-schema.html).

For an example configuration see [test.pr](./test.pr).

The was once an abstraction of the Nix worker socket that could intermediate between clients and the worker but that code has been removed, refer to git history for that.
