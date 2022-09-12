We've now been working on our rewrite of Nix, [Tvix][], on-and-off for
over a year.

Of course, for many of us, it's been a pretty turbulent time period.
While steady progress has been made, we haven't really had the
bandwidth to communicate and publicise what has been going on - this
blog post aims to rectify that!

## Nix language evaluator

The most significant progress in the last months has been made on our
Nix language evaluator. To address a big question right away: Yes, you
can play with it - in [Tvixbolt][]!

We developed the evaluator to the current state by enumerating the
various problems we were likely to encounter, and writing small-scale
solutions to them before assembling them into a whole. Due to the
nature of this process, we briefly ended up with a very large private
source tree, which we [integrated]() into our monorepo in the last
couple of weeks.

This process was slow mostly due to code review bandwidth, but
remember that we are just volunteers and such bottlenecks are to be
expected!

Most of this code was written or reviewed by [tazjin][], [grfn][]
and [sterni][].

### So, what's working now?

The answer is *most things*! There are two major feature areas that
are majorly unfinished:

1. The majority of Nix's `builtins` are not yet implemented (including
   fundamental ones such as `import` and `derivation`).

2. Recursive attribute sets (`rec`) are not yet implemented.

In both cases we have mostly figured out how to do the remaining work
and it is simply a question of time until we've done it. Progress is
steady and can of course be tracked [in the source][src] (viewer
without Javascript [here][src-noscript]).

At the same time, we've already implemented a variety of basics that
are hopefully going to have a huge impact further down, such as:

* The Tvix compiler is built to be able to emit warnings & errors
  without failing early, as well as retaining as much source
  information as possible. This will enable developer tooling, such as
  language servers, to be based on Tvix.

* The Tvix compiler performs very in-depth scope analysis, which
  allows it to both generate efficient bytecode for accessing
  identifiers, as well as alert users about problems in their code
  before runtime.

* The runtime supports tail-call optimisation in many (but not all
  (yet!)) cases, allowing us to evaluate many recursive expressions in
  constant stack space.

### How does this all work?

Tvix's evaluator is implemented using a custom abstract machine with a
very Nix-specific instruction set, as well as a compiler that
traverses a parsed Nix AST to emit this bytecode and perform a set of
optimisations and other analysis.

This is all written in Rust (of course) and is currently made up of
less than 5000 lines of code (some of which look deceptively simple,
especially around scope-handling!).

We run the evaluator against many custom tests we have written, as
well as against the upstream Nix test suite (which we do not yet pass,
but are working towards).

### What's next for tvix-eval?

Apart from the missing language features outlined above, the next
steps are:

* Comprehensive benchmarking. We are standing up an infrastructure for
  continuous benchmarking to measure the impact of changes, and to be
  able to identify and optimise hotspots.

* Implementing known optimisations. There are some areas of the code
  where we are aware of (significant) possible speed gains, but we are
  holding off of implementing them until the evaluator is feature
  complete and passes the Nix language test suite.

* Finishing our language specification. Based on the behaviours we've
  learned, we are writing a specification of the Nix language that
  captures its various (sometimes subtly tricky) behaviours.

Once we're nearing completion of the language feature set, focus is
likely to shift towards the other areas of Tvix.

## The Other Areas of Tvix

Speaking of these other areas (most importantly, the builder and store
implementation), some progress has been made there also.

While we haven't begun piecing together the final implementations,
[flokli][] and [adisbladis][] have been hard at work on [go-nix][]
which aims to implement many of the low-level primitives required for
Nix (hashing and encoding schemes, archive formats, reference scanning
...).

We're looking forward to being able to tell you more about this in the
next update!

## Outro ...

We'd be delighted to onboard new contributors to Tvix! Please take a
look at the main [TVL page](https://tvl.fyi) to find out how to get in
touch with us if you'd like to join!

Thanks also, of course, to [NLNet](https://nlnet.nl/) for sponsoring
some of this work!

And finally, we would like to thank and pay our respects to jD91mZM2 -
the original author of
[rnix-parser](https://github.com/nix-community/rnix-parser) - who
sadly passed away. We use `rnix-parser` in our compiler, and its
well-designed internals (also thanks to its new maintainers!) have
saved us a lot of time.

That's it for this update, go play with [Tvixbolt][], tell us about
the weird ways in which you break it, get in touch, and we'll see you
around!

[Tvix]: https://tvl.fyi/blog/rewriting-nix
[Tvixbolt]: https://tvixbolt.tvl.su
[integrated]: https://cl.tvl.fyi/q/status:merged+%2522tvix/eval%2522+mergedbefore:2022-09-09
[src]: https://cs.tvl.fyi/depot/-/tree/tvix/eval
[src-noscript]: https://code.tvl.fyi/tree/tvix/eval
[tazjin]: https://tazj.in
[grfn]: https://gws.fyi/
[sterni]: https://github.com/sternenseemann
[go-nix]: https://github.com/nix-community/go-nix
[flokli]: https://github.com/flokli
[adisbladis]: https://github.com/adisbladis
