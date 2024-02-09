It's already been way too long since the last update here. It doesn't mean
nothing has moved forward since, in fact a lot of things happened, but while
there's been a talk about Tvix at [NixCon 2023][nixcon2023], as well as a
[Nix Developer Dialogue Interview][nix-dev-dialogues-tvix], we never found the
time to write something down - time to change that :-)

## Evaluation regression testing
Most of the Evaluator work has been driven by evaluating nixpkgs, and ensuring
we "evaluate it the same", which ultimately means we produce the same builds and
end up with the same store paths.

We don't build things yet, but at least for nixpkgs (which doesn't do any IFD),
it's possible to just peek at the `outPath` (and `drvPath`) of a package, and
compare the calculated store path(s) with what Nix produces to determine if they
use the same build recipe (for the package as well as all of its dependencies)
[^1].

We added some "integration tests" to our CI ensuring we keep getting the same
output and drv paths as Nix, and already have quite complicated expressions in
there and passing, like firefox, which exercises some of the more hairy bits in
nixpkgs (like cross-compilation infrastructure for WASM). Yay!

Even though we're not getting into very fine-grained optimization until we're
sure Tvix evaluates nixpkgs in a compatible fashion, we still want to have an
idea about evaluation performance, and how it changes over time.
To do this, we extended our benchmarks and wired them into
[Windtunnel][windtunnel], which now regularly runs benchmarks and provides
graphs of how the benchmark results change over time, from commit to commit.

In the future, we plan to extend this to also run this as a part of code review,
before changes are applied to our main branch. This will make it easier to see
changes in performance right in the web interface, without having
to do a manual benchmark locally before and after the change.

## `builtins.derivation[Strict]`, ATerms and output path calculation
These two are obviously needed to compare any derivation. As an interesting side
note, in Nixcpp, the former is defined by a piece of
[bundled-in-Nix `.nix` code][nixcpp-builtins-derivation], that massages some
parameters around and then calls the "native" `derivationStrict` - so
implementing them required having the necessary tooling in Tvix to have builtins
defined in `.nix` source code).

`builtins.derivation[Strict]` returns an attribute set with the previously
mentioned `outPath` and `drvPath` paths. Implementing that correctly required
implementing output path calculation the same way as Nix does (bit-by-bit).

Very little of how the output path calculation works is documented anywhere
in Nixlang. It uses a subset of [ATerm][aterm] internally, produces
"fingerprints" containing hashes of these ATerms, which are then hashed again.
The intermediate hashes are not printed out anywhere (except if you [patch nix]
[nixcpp-patch-hashes] to do so).

We already did parts of this correctly while starting this work on
[go-nix][go-nix-outpath] some while ago, but found some more edge cases and
ultimately came up with a nicer interface than there.

All the Derivation internal data model, ATerm serialization and output path
calculation have been sliced out into a more general-purpose
[nix-compat][nix-compat-derivation] crate, alongside with more documentation
unit tests and a Derivation ATerm parser, so hopefully this will now be more
accessible for everyone now.

Note our builtin does *not* yet persist the Derivation anywhere "on
disk" (though we have a debug CL that does write it to a temporary directory,
in case we want to track down differences).

## `tvix-[ca]store`
Tvix now has a store implementation!

### The Nix model
Inside Nix, store path contents are normally hashed and communicated in NAR
format, which is very coarse and often wasteful - a single bit of change in one
file in a large store path causes a new NAR file to be uploaded to the binary
cache, which then needs to be downloaded.

Additionally, identifying everything by the SHA256 digest of its NAR
representation makes Nix store paths very incompatible with other
content-addressed systems, as it's a very Nix-specific format.

### The more granular Tvix model
After experimenting with some concepts and ideas in Golang, mostly around how to
improve binary cache performance [^3], both on-disk as well as over the network,
we settled on a more granular, content-addressed and general-purpose format.

Internally, it behaves very similar to how git handles tree objects, except
blobs are identified by their raw BLAKE3 digests rather than some custom
encoding, and similarly, tree/directory objects use the BLAKE3 digest of its
canonical protobuf serialization as identifiers.

This provides some immediate benefits:
 - We only need to keep the same data once, even if it's used across different
   store paths.
 - Transfers can be more granular and only need to fetch the data that's
   needed. Due to everything being content-addressed, it can be fetched from
   anything supporting BLAKE3 digests, immediately making it compatible with
   other P2P systems (IPFS blake3 blobs, …), or general-purpose
   content-addressed caches ([bazel-remote]).

There's a lot more details about the data model, certain decisions etc. in
[the docs][castore-docs].

### Compatibility
We however still want to stay compatible with Nix, as in calculating
"NAR-addressed" store paths the same, support substituting from regular Nix
binary caches, as well as storing all the other additional metadata about store
paths.

We accomplished this by splitting the two different concerns into two separate
`tvix-store` and `tvix-castore` crates, with the former one holding all
Nix-specific metadata and functionality, and the latter being a general-purpose
content-addressed blob and filesystem tree storage system, which is usable in a
lot of contexts outside of Tvix too. For example, if you want to use
tvix-castore to write your own git alternative, or provide granular and
authenticated access into large scientific datasets, you could!

### Backends
In addition to a gRPC API and client bindings, there's support for local
filesystem-based backends, as well as for sled, an embedded K/V database.

We're also currently working on a backend supporting most common object
storages, as well as on more granular seeking and content-defined chunking for
blobs.

### FUSE/virtiofs
A tvix-store can be mounted via FUSE, or exposed through virtiofs [^4].
While doing the obvious thing - allowing mounting and browsing the contents
of the store, this will allow lazy substitution of builds on remote builders, be
in containerized or virtualized workloads.

We have an [example][tvix-boot-readme] in the repository seeding gnu hello into
a throwaway store, then booting a MicroVM and executing it.

### nar-bridge, bridging binary caches
`nar-bridge` and the `NixHTTPPathInfoService` bridge `tvix-[ca]store` with
existing Nix binary caches and Nix.

The former exposes a `tvix-[ca]store` over the common Nix HTTP Binary Cache
interface (both read and write).

The latter allows Tvix to substitute from regular Nix HTTP Binary caches,
unpacking NARs and ingesting them on-the-fly into the castore model.
The necessary parsers for NARInfo, signatures etc are also available in the
[nix-compat crate][nix-compat-narinfo].

## EvalIO / builtins interacting with the store more closely
tvix-eval itself is designed to be quite pure when it comes to IO - it doesn't
do any IO directly on its own, but for the very little IO functionality it
does as part of "basic interaction with paths"[^2] (like importing other
`.nix` files), it goes through an `EvalIO` interface, which is provided to the
Evaluator struct on instantiation.

This allows us to be a bit more flexible with how IO looks like in practice,
which becomes interesting for specific store implementations that might not
expose a POSIX filesystem directly, or targets where we don't have a filesystem
at all (like WASM).

Using the `EvalIO` trait also allows avoiding the `tvix-eval` crate to get too
strongly coupled to a specific store implementation, hashing scheme etc [^2].
As we can extend the set of builtins available to the evaluator with "foreign
builtins", these can live in other crates.

Following this pattern, we started implementing some of the "basic" builtins
that deal with path access in `tvix-eval`, like:

 - `builtins.pathExists`
 - `builtins.readFile`

We also recently started working on more complicated builtins like
`builtins.filterSource` and `builtins.path`, which are also used in nixpkgs.

Both import a path into the store, and allow passing a Nix expression that's
used as a filter function for each path. `builtins.path` can also ensuring the
imported contents match a certain hash.

This required the builtin to interact with the store and evaluator in a very
tight fashion, as the filter function (written in Nix) needs to be repeatedly
executed for each path, and its return value is able to cause the store to skip
over certain paths (which it previously couldn't).

Getting the abstractions right there required some back-and-forth, but the
remaining changes should land quite soon.

## Catchables / tryEval
As you may know, Nix has a limited exception system for dealing with
user-generated errors: `builtins.tryEval` can be used to detect if an expression
fails (if `builtins.throw` or `assert` are used to generate it). This feature
requires extra support in any Nix implementation, as errors may not necessarily
cause the Nix program to abort.

The C++ Nix implementation just reuses the C++ language-provided Exception
system for `builtins.tryEval` which Tvix can't (even if Rust had an equivalent
system):
In C++ Nix the runtime representation of the program in execution corresponds
to the Nix expression tree of the relevant source files. This means that an
exception raised in C++ code will automatically bubble up correctly since the
C++ and Nix call stacks are equivalent to each other.
Tvix compiles the Nix expressions to a byte code program which may be mutated
by extra optimization rules (for example, we hope to eliminate as many thunks as
possible in the future). This means that such a correspondence between Nix and
the (in this case) VM runtime is not guaranteed.

Previously, `builtins.tryEval` (which is implemented in Rust and can access VM
internals) just allowed the VM to recover from certain kinds of errors. This
proved to be insufficient as it [blew up as soon as a `builtins.tryEval`-ed thunk
is forced again][tryeval-infrec]—extra bookkeeping was needed. As a
solution, we now store thunk evaluation errors that can be recovered from as
`Value::Catchable` which mitigates this problem.

As you can imagine, storing evaluation failures as "normal" values quickly leads
to all sorts of bugs because most VM/builtins code is written with only ordinary
values like attribute sets, strings etc. in mind.
While ironing those out, we made sure to supplement those fixes with as many
test cases for `builtins.tryEval` as possible. This will hopefully prevent any
regressions if or rather when we touch this system again. We already have some
ideas for replacing the `Catchable` value type with a cleaner representation.

## String contexts
For a long time, we had the [working theory][refscan-string-contexts]
of being able to get away with not implementing string contexts, but
instead do reference scanning on a set of "known paths" (and not implement
`builtins.unsafeDiscardStringContext`).

Unfortunately, we ultimately discovered we won't be able to do that, mostly due to a
[bug in Nix][string-contexts-nix-bug] that's worked around in the nixpkgs'
`stdenv.mkDerivation` implementation, but impossible to fix in Nix without
breaking all hashes.

So we recently added support for string contexts into our `NixString`
implementation, implemented the (`unsafeDiscardStringContext`, `getContext`)
builtins, as well as some more unit tests that introspect string context
behaviour of various builtins.

## Strings as bstr
C++ nix uses C-style zero-terminated char pointers to represent strings
internally - however, until recently, Tvix has used Rust `String` and `str` for
string values. Since those are required to be valid utf-8, we haven't been able
to properly represent all the string values that Nix supports.

We recently converted our internal representation to `BString`, which allows
treating a `Vec<u8>` as a "morally-string-like" value.

## JSON/TOML/XML
We added support for the `toJSON`, `toXML`, `fromJSON` and `fromTOML` builtins.

`toXML` is particularly exciting, as it's the only format that allows expressing
(partially applied) functions. It's also used in some of Nix' own test suite, so
we can now include these in our unit test suite (and pass, yay!).

## Builder protocol, drv->builder
Some work went into the Builder protocol, and how Tvix represents builds
internally.

Nix uses Derivations (in A-Term) as nodes in its build graph, but it refers to
other store paths used in that build simply by these store paths *only*, and
doesn't encode their expected *content hashes* anywhere.

In Nix, this poses a big problem as soon as these builds are scheduled on remote
builders: Builds scheduled to a builder are not truly hermetic, but rely on that
referred store path to actually have the same contents as the machine
orchestrating the build (or at least very similar).

If a package is not binary reproducible, this can lead to so-called
[frankenbuilds][frankenbuild].
Even ignoring this problem, we still rely on state (the exact contents of that
output in the remote builders' Nix Store), and making them appear there (if they
can't be substituted) requires the one scheduling the build to copy them over,
and be a trusted user.

We wanted to eliminate this hermiticity problem in the internal data model
that Tvix uses to manage builds, by not relying on external state, and instead
explicitly tracking both build inputs as well as produced outputs by their
contents, too.

---

That's it for now!

---
[^1]: Why this means it's using the same build recipes is due to how the output
      path calculation for input-addressed paths works, more of that in the
      next section.
[^2]: That's the same reason why `builtins.derivation[Strict]` also lives in
      `tvix-glue`, not in `tvix-eval`.
[^3]: See [nix-casync](https://discourse.nixos.org/t/nix-casync-a-more-efficient-way-to-store-and-substitute-nix-store-paths/16539)
      for one example - investing content-defined chunking (while still keeping
      the NAR format)
[^4]: Strictly speaking, not limited to tvix-store - literally anything
      providing a listing into tvix-castore nodes.

[aterm]:                      http://program-transformation.org/Tools/ATermFormat.html
[bazel-remote]:               https://github.com/buchgr/bazel-remote/pull/715
[castore-docs]:               https://cs.tvl.fyi/depot/-/blob/tvix/castore/docs
[frankenbuild]:               https://blog.layus.be/posts/2021-06-25-frankenbuilds.html
[go-nix-outpath]:             https://github.com/nix-community/go-nix/blob/93cb24a868562714f1691840e94d54ef57bc0a5a/pkg/derivation/hashes.go#L52
[nix-compat-derivation]:      https://docs.tvix.dev/rust/nix_compat/derivation/struct.Derivation.html
[nix-compat-narinfo]:         https://docs.tvix.dev/rust/nix_compat/narinfo/index.html
[nix-dev-dialogues-tvix]:     https://www.youtube.com/watch?v=ZYG3T4l8RU8
[nixcon2023]:                 https://www.youtube.com/watch?v=j67prAPYScY
[nixcpp-builtins-derivation]: https://github.com/NixOS/nix/blob/49cf090cb2f51d6935756a6cf94d568cab063f81/src/libexpr/primops/derivation.nix#L4
[nixcpp-patch-hashes]:        https://github.com/adisbladis/nix/tree/hash-tracing
[refscan-string-contexts]:    https://inbox.tvl.su/depot/20230316120039.j4fkp3puzrtbjcpi@tp/T/#t
[store-docs]:                 https://cs.tvl.fyi/depot/-/blob/tvix/store/docs/api.md
[string-contexts-nix-bug]:    https://github.com/NixOS/nix/issues/4629
[tryeval-infrec]:             https://b.tvl.fyi/issues/281
[tvix-boot-readme]:           https://cs.tvl.fyi/depot/-/blob/tvix/boot/README.md
[why-string-contexts-now]:    https://cl.tvl.fyi/c/depot/+/10446/7/tvix/eval/docs/build-references.md
[windtunnel]:                 https://staging.windtunnel.ci/tvl/tvix
