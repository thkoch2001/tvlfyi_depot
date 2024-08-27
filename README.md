# Syndicated Nix Actor

An actor for interacting with the [Nix](https://nixos.org/) daemon via the [Syndicated Actor Model](https://syndicate-lang.org/).

See [protocol.prs](./protocol.prs) for the Syndicate protocol [schema](https://preserves.dev/preserves-schema.html).

## Evaluation state as entity capabililties

The actor exposes on its initial capability a gatekeeper that resolves requests in the form `<nix { lookupPath: [ … ], store: … } >`.

The resolved entity is an evaluation state that responds to the assertions `<eval @expr string @args any @result #:Result>` as well as observation of literal values via the dataspace protocol.
The evaluation state is initialized with the value `nil` and is advanced with Nix functions in the form of `prev: args: body` with a type of `Any -> Any -> Any`.

To evaluate the `hello` package from Nixpkgs one could use an assertion like `<eval "_: pkgName: builtins.getAttr pkgName (import <nixpkgs> {})" "hello" #:…>` which would assert back a new assertion state at `hello`.

The evaluation state represents a lazy Nix expression and must be "realised" to become a physical store path.
Asserting `<realise-string @result #:Result>` to an evaluation state will return a string with its realized closure at the evaluation store.

With the exception of observations the result value of `<ok @value any>` or `<error @message any>` is used for response assertions.

Dataspace observations work over evaluation state.
In the example case of an evaluation state positioned at the `hello` package the observation of `{ hello: { meta: { license: { shortName: ? } } } }` would capture the value "gpl3Plus".
If an attrset contains an `outPath` attribute then the value of `outPath` is captured in place of the attrset.
This is to avoid traversing deeply nested and recursive Nix values.

### TODO
Realise store-paths from expressions using local and remote builds.

## Worker protocol

The was once an abstraction of the Nix worker socket that could intermediate between clients and the worker but that code has been removed, refer to git history for that.
