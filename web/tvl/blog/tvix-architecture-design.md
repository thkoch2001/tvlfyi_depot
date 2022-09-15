# Tvix Architecture Design

This blogpost is showing the desired architecture used in Tvix, to describe how
various components interact with each other.

One of our goals with Tvix is the decoupling of the various components that we
think explicitly exist in Nix, but currently lack well-defined interfaces in
between.

This facilitates having multiple implementations of components in various
programming languages. New ideas can be explored without having to perform
massive refactorings across the entire codebase.
We can experiment with new language frontends / store protocols / builders
without impacting the existing codebase too much.

## Components
We ended up with three main components, all speaking well-defined interfaces,
connected together by an additional "coordinator" component:

### Evaluator
An evaluator consumes (Nix) Code and emits derivations, which describe how that
specific node in the graph can be built.

At least for Nix, imports of `.nix` files can happen:

 - Specified in the `.nix` file that's requested to be built (as well as
   transitively in these files)
 - Import of a path including some string interpolation, requiring a build to
   be done before being able to import (commonly known as IFD)

This means the evaluator needs some sort of file-level access, both to files in
the "working directory", as well as access to files produced from a build.

It also needs a way to request the the build of a derivation to be triggered
(to distinguish between the equivalent of a `nix-instantiate` vs `nix-build`).
In the case of IFD, even a `nix-instantiate` can trigger builds of the
to-be-imported paths.

As of now, the Tvix evaluator was pure and didn't implement any
builtins/language constructs interacting with the store. These interactions can
nicely be hidden inside the builtin implementations itself, without creeping
too much into the generic evaluator codebase.

### (Nix)Store
A store takes care of storing (Nix) output paths, their contents and metadata,
so they can be provided to Evaluators and Builders.

Note it doesn't store Derivations. [^store-no-drv]

Conceptionally, Binary Caches are also just stores speaking the same protocol,
and different instances of stores can be "composed together" under the same
interface.

In the `go-nix` codebase, there has been a lot of experimentation on different
efficient internal representations of store contents and externally-facing
interfaces. More of the internals of this will be covered in a followup
article.

Essentially, on the outside, a (Nix)Store exposes:
 - an interface to ingest output paths (alongside with metadata)
 - an interface to give them back
 - an interface to check if the output path exist

### Builder
A builder consumes build requests, and builds them. We decided to use
[CRI](https://kubernetes.io/docs/concepts/architecture/cri/), rather than our
own sandboxing code, and were successfully able to build some dummy derivations
in some proof-of-concept code.

The builder needs file-level access to a store, to provide any build
dependencies required at the runtime of the build.

Once the build succeeded, the build results can be uploaded to a store.

### Coordinator
The coordinator is what connects these three different components together,
depending on the local configuration.

This is what's driving the evaluator, collects derivations, triggers builds in
the builder and signs and uploads builds to the store.

It's also what's doing the output path calculation, and only scheduling builds
if an output path hasn't been realized already.


### Open Questions / Outlook
Some of these components are very generically useful:

 - The builder code doesn't necessarily need to know about the various
   delicacies of a Nix-specific sandboxing environment, but can consume more
   generic build requests (see also
   [here](https://discourse.nixos.org/t/a-proposal-for-replacing-the-nix-worker-protocol/20926/22))
 - The store doesn't need to know how output paths are calculated.

This means, it looks like it becomes feasible to also support other frontend
languages (like Guile or Starlark), or other store models (intensional store).


[^store-no-drv]: Conceptionally, there's nothing preventing it from storing
  derivations, but there's few reason to do so. This has also surfaced every
  once in a while in Nixcpp.
