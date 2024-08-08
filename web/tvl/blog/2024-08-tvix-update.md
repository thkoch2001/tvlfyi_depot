It's already been around half a year since
[the last Tvix update][2024-02-tvix-update], so time for another one!

Note: This blog post is intended for a technical audience that is already
intimately familiar with Nix, and knows what things like derivations or store
paths are. If you're new to Nix, this will not make a lot of sense to you!

## Builds
A long-term goal is obviously to be able to use the expressions in nixpkgs to
build things with Tvix. We made progress on many places towards that goal:

### Drive builds on IO
As already explained in our [first blog post][blog-rewriting-nix], in Tvix, we
want to make IFD a first-class citizen without significant perf cost.

Nix tries hard to split Evaluation and Building into two phases, visible in
the `nix-instantiate` command which produces `.drv` files in `/nix/store` and
the `nix-build` command which can be invoked on such `.drv` files without
evaluation.
Scheduling (like in Hydra) usually happens by walking the graph of `.drv` files
produced in the first phase.

As soon as there's some IFD along the path, everything until then gets built in
the Evaluator (which is why IFD is prohibited in nixpkgs).

Tvix does not have two separate "phases" in a build, only a graph of unfinished
Derivations/Builds and their associated store paths. This graph does not need
to be written to disk, and can grow during runtime, as new Derivations with new
output paths are discovered.

Build scheduling happens continuously with that graph, for everything that's
really needed, when it's needed.

We do this by only "forcing" the realization of a specific store path if the
user ultimately wants that specific result to be available on their system, and
transitively, if something else wants it. This includes IFD in a very elegant
way.

We want to play with this approach as we continue on bringing our build
infrastructure up.

### Fetchers
There's a few Nix builtins that allow describing a fetch (be it download of a
file from the internet, clone of a git repo). These needed to be implemented
for completeness. We implemented pretty much all downloads of Tarballs, NARs and
plain files, except git repositories, which are left for later.

Instead of doing these fetches immediately, we added a generic `Fetch` struct
that allows describing such fetches *before actually doing them*, similar to
being able to describe builds, and use the same "Drive builds on IO" machinery
to delay these fetches to the point where it's needed. We also show progress
bars when doing fetches.

Very early, during bootstrapping, nixpkgs relies on some `builtin:fetchurl`
"fake" Derivation, which has some special handling logic in Nix. We implemented
these quirks, by converting it to our generic `Fetch` struct and dealing with
it there in a consistent fashion.

### More fixes, Refscan
With the above work done, and after fixing some small bugs [^3], we were already
able to build some first few store paths with Tvix and our `runc`-based builder
🎉!

We didn't get too far though, as we still need to implement reference scanning,
so that's next on our TODO list for here. Stay tuned for further updates there!

## Eval correctness & Performance
As already written in the previous update, we've been evaluating parts of
`nixpkgs` and ensuring we produce the same derivations. We managed to find and
fix some correctness issues there.

Even though we don't want to focus too deep on performance improvements
until all features of Nix are properly understood and representable with our
architecture, there's been some work on removing some obvious and low-risk
performance improvements. Expect a detailed blog post around that soon after
this one!

## Tracing / O11Y Support
Tvix got support for Tracing, and is able to emit spans in
[OpenTelemetry][opentelemetry]-compatible format.

This means, if the necessary tooling is set up to collect such spans [^1], it's
possible to see what's happening inside the different components of Tvix across
process (and machine) boundaries.

Tvix now also propagates trace IDs via gRPC and HTTP requests [^2], and
continues them if receiving such ones.

As an example, this allows us to get "callgraphs" on how a tvix-store operation
is processed through a multi-node deployment, and find bottlenecks and places to
optimize performance for.

Currently, this is compiled in by default, trying to send traces to an endpoint
at `localhost` (as per the official [SDK defaults][otlp-sdk]). It can
be disabled by building without the `otlp` feature, or running with the
`--otlp=false` CLI flag.

This piggy-backs on the excellent [tracing][tracing-rs] crate, which we already
use for structured logging, so while at it, we improved some log messages and
fields to make it easier to filter for certain types of events.

We also added support for sending out [Tracy][tracy] traces, though these are
disabled by default.

Additionally, some CLI entrypoints can now report progress to the user!
For example, when we're fetching something during evaluation
(via `builtins.fetchurl`), or uploading store path contents, we can report on
this. See [here][asciinema-import] for an example.

We're still considering these outputs as early prototypes, and will refine them as
we go.

## tvix-castore ingestion generalization
We spent some time refactoring and generalizing tvix-castore importer code.

It's now generalized on a stream of "ingestion entries" produced in a certain
order, and there's various producers of this stream (reading through the local
filesystem, reading through a NAR, reading through a tarball, soon: traversing
contents of a git repo, …).

This prevented a lot of code duplication for these various formats, and allows
pulling out helper code for concurrent blob uploading.

## Documentation reconcilation
Various bits and pieces of documentation have previously been scattered
throughout the Tvix codebase, which wasn't very accessible and quite confusing.

These have been consolidated into a mdbook (at `//tvix/docs`).

We plan to properly host these as a website, hopefully providing a better introduction
and overview into Tvix, while adding more content over time.

## `nar-bridge` RIIR
While the golang implementation of `nar-bridge` did serve us well for a while,
it being the only remaining non-Rust part was a bit annoying.

Adding some features there meant they would not be accessible in the rest of
Tvix - and the other way round.
Also, we could not open data stores directly from there, but always had to start
a separate `tvix-store daemon`.

The initial plans for the Rust rewrite were already made quite a while ago,
but we finally managed to finish implementing the remaining bits. `nar-bridge`
is now fully written in Rust, providing the same CLI experience features and
store backends as the rest of Tvix.

## `crate2nix` and overall rust Nix improvements
We landed some fixes in [crate2nix][crate2nix], the tool we're using to for
per-crate incremental builds of Tvix.

It now supports the corner cases needed to build WASM - so now [Tvixbolt]
[tvixbolt] is built with it, too.

We also fixed some bugs in how test directories are prepared, which unlocked
running some more tests for filesystem related builtins such as `readDir` in our test suite.

Additionally, there has been some general improvements around ensuring various
combinations of Tvix feature flags build (now continuously checked by CI), and
reducing the amount of unnecessary rebuilds, by filtering non-sourcecode files
before building.

These should all improve DX while working on Tvix.

## Store Composition
Another big missing feature that landed was Store Composition. We briefly spoke
about the Tvix Store Model in the last update, but we didn't go into too much
detail on how that'd work in case there's multiple potential sources for a store
path or some more granular contents (which is pretty much always the case
normally, think about using things from your local store OR then falling back to
a remote place).

Nix has the default model of using `/nix/store` with a sqlite database for
metadata as a local store, and one or multiple "subsituters" using the Nix HTTP
Binary Cache protocol.

In Tvix, things need to be a bit more flexible:
 - You might be in a setting where you don't have a local `/nix/store` at all.
 - You might want to have a view of different substituters/binary caches for
   different users.
 - You might want to explicitly specify caches in between some of these layers,
   and control their config.

The idea in Tvix is that you'll be able to combine "hierarchies of stores" through
runtime configuration to express all this.

It's currently behind a `xp-store-composition` feature flag, adding a
`--experimental-store-composition` CLI arg which can point to a TOML file
specifying the composition configuration. This serves as an alternative to the CLI args for the
three (single) stores.

We're still not 100% sure how to best expose this functionality, in terms of the appropriate level of granularity, in a user-friendly format.

There's also some more combinators and refactors missing, but please let us
know your thoughts!

## Contributors
There's been a lot of progress, which would not have been possible without our
contributors! Be it a small drive-by contributions, or large efforts, thank
you all!

 - Adam Joseph
 - Alice Carroll
 - Aspen Smith
 - Ben Webb
 - binarycat
 - Brian Olsen
 - Connor Brewster
 - Daniel Mendler
 - edef
 - Edwin Mackenzie-Owen
 - espes
 - Farid Zakaria
 - Florian Klink
 - Ilan Joselevich
 - Luke Granger-Brown
 - Markus Rudy
 - Matthew Tromp
 - Moritz Sanft
 - Padraic-O-Mhuiris
 - Peter Kolloch
 - Picnoir
 - Profpatsch
 - Ryan Lahfa
 - Simon Hauser
 - sinavir
 - sterni
 - Steven Allen
 - tcmal
 - toastal
 - Vincent Ambo
 - Yureka

---

That's it again, try out Tvix and hit us up on IRC or on our mailing list if you
run into any snags, or have any questions.


[^1]: Essentially, deploying a collecting agent on your machines, accepting
      these traces.
[^2]: Using the `traceparent` header field from https://www.w3.org/TR/trace-context/#trace-context-http-headers-format
[^3]: like `builtins.toFile` not adding files yet, or `inputSources` being missed initially, duh!)

[2024-02-tvix-update]:        https://tvl.fyi/blog/tvix-update-february-24
[opentelemetry]:              https://opentelemetry.io/
[otlp-sdk]:                   https://opentelemetry.io/docs/languages/sdk-configuration/otlp-exporter/
[tracing-rs]:                 https://tracing.rs/
[tracy]:                      https://github.com/wolfpld/tracy
[asciinema-import]:           https://asciinema.org/a/Fs4gKTFFpPGYVSna0xjTPGaNp
[blog-rewriting-nix]:         https://tvl.fyi/blog/rewriting-nix
[crate2nix]:                  https://github.com/nix-community/crate2nix
[tvixbolt]:                   https://bolt.tvix.dev/
