Nix, the programming language, is often declared by people not to be
a general-purpose language. This is true - for the most part.

There are various examples of software written in Nix that is not just
package definitions, for example the NixOS module system, or TVL
projects like [`//nix/yants`](https://at.tvl.fyi/?q=%2F%2Fnix%2Fyants)
and [`//web/bubblegum`](https://at.tvl.fyi/?q=%2F%2Fweb%2Fbubblegum).

Some other projects, like the code that [generates build
instructions](https://at.tvl.fyi/?q=%2F%2Fops%2Fpipelines) for TVL's
[CI setup](https://tvl.fyi/builds), are closer to the classic domain
of Nix but still have much higher complexity than a plain package
definition.

Whichever project you pick, they all suffer from the same problem:
Evaluating the Nix language is currently very slow. It takes us close
to a minute to create the CI instructions for our monorepo at the
moment despite it being a plain Nix evaluation and running our
Nix-native build systems for
[Go](https://code.tvl.fyi/about/nix/buildGo) and [Common
Lisp](https://code.tvl.fyi/about/nix/buildLisp) takes much more time
than we would like.

Some time last year a few of us got together and started investigating
ways to modernise the current architecture of Nix and figure out ways
to improve the speed of some of the components. We created over [250
commits](https://cl.tvl.fyi/q/topic:tvix) in our fork of the Nix 2.3
codebase at the time, tried [performance
experiments](https://cl.tvl.fyi/c/depot/+/1123/) aimed at improving
the current evaluator and fought [gnarly
bugs](https://cl.tvl.fyi/c/depot/+/1504).

After a while we realised that we are treading water: Some of our
ideas are too architecturally divergent from Nix to be done on top of
the existing codebase, and the memory model of Nix causes significant
headaches when trying to do any kind of larger change.

We needed an alternative approach and started brainstorming on a bent
whiteboard in a small flat in Hurghada, Egypt.

![flokli & tazjin brainstorming](https://static.tvl.fyi/latest/flokli_tazjin_tvix.webp)

<!-- TODO(tazjin): Wait for adisbladis approval to use this picture
![adisbladis & tazjin brainstorming](https://static.tvl.fyi/latest/adisbladis_tazjin_tvix.webp)
-->

We came up with [the
architecture](https://code.tvl.fyi/about/tvix/docs/components.md) for
our new project: Tvix, a reimagined Nix. Half a year later we now have
funding for this project [from NLNet](https://nlnet.nl/project/Tvix/)
(thanks!) and are ready to start implementing it.

Our goals are:

* Creating an alternative implementation of Nix that is **fully
  compatible with nixpkgs**, as we believe that most of the value of
  Nix currently lives in the massive amount of community contributions
  to nixpkgs.
* A new Nix language implementation with a more efficient interpreter
  using a simple abstract machine, and without the strict separation
  between evaluation and build phases
* A modular architecture in which components are replaceable: For
  example, it should be possible to write a Tvix frontend that plugs
  in the Guix language, to use arbitrary OCI-compatible build
  sandboxes and to replace the store implementation.

Tvix is not intended to *replace* Nix, instead we want to improve the
ecosystem by offering a diversity of implementations. However, we do
not intend to support experimental features.

As things ramp up we will be posting more information on this blog,
for now you can keep an eye on
[`//tvix`](https://cs.tvl.fyi/depot/-/tree/tvix) in the TVL monorepo,
subscribe to [our feed](https://tvl.fyi/feed.atom), and join us on IRC
to contribute your ideas and feedback.

Stay tuned!
