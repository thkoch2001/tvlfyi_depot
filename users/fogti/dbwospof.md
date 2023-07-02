# distributed build without single points of failure

## problem statement
> If we want to distribute a build across several build nodes, and want to avoid
> a "single point of failure", what needs to be considered?

## motivation

* distribute the build across several build nodes, because some packages take
  extremely long to build
  (e.g. `firefox`, `thunderbird`, `qtwebengine`, `webkitgtk`, ...)
* avoid a centralised setup like e.g. with Hydra, because we want to keep using
  an on-demand workflow as usual with Nix
  (e.g. `nixos-rebuild` on each host when necessary).

## list of abbreviations

<dl>
  <dt>CA</dt>		<dd>content-addressed</dd>
  <dt>drv</dt>		<dd>derivation</dd>
  <dt>FOD</dt>		<dd>fixed-output derivation</dd>
  <dt>IHA</dt>		<dd>input-hash-addressed</dd>
  <dt>inhash</dt>	<dd>input hash</dd>
  <dt>outhash</dt>	<dd>output hash</dd>
</dl>

## build graph

The build graph can't be easily distributed. It is instead left on the coordinator,
and the build nodes just get individual build jobs, which just consist of
derivations (and some information about how to get the inputs from some central
or distributed store (e.g. Ceph), this may be transmitted "out of band").

## inhash-exclusive

It is necessary that each derivation build is exclusive in the sense that
the same derivation is never build multiple times simultaneously, because
this otherwise either wastes compute resources (obviously) and, in the case
of non-deterministic builds, increases complexity
(the store needs to decide which result to prefer, and the build nodes with
"losing" build results need to pull the "winning" build results from the store,
replacing the local version). Although this might be unnecessary in case
of IHA drvs, enforcing it always reduces the amount of possible suprising
results when mixing CA drvs and IHA drvs.

## what can be meaningfully distributed

The following is strongly opinionated, but I consider the following
(based upon the original build graph implementation from yzix 12.2021):
* We can't push the entire build graph to each build node, because they would
  overlap 100%, and thus create extreme contention on the inhash-exclusive lock
* We could try to split the build graph into multiple parts with independent
  inputs (partitioning), but this can be really complex, and I'm not sure
  if it is worth it... This also basically excludes the yzix node types
  [ `Eval`, `AssertEqual` ] (should be done by the evaluator).
  Implementing this option however would make an abort of a build graph
  (the simple variant does not kill running tasks,
  just stop new tasks from being scheduled) really hard, and complex to get right.
* It does not make sense to distribute "node tasks" across build nodes which
  almost exclusively interact with the store, and are not CPU-bound, but I/O bound.
  This applies to most, if not all, useful FODs. It applies to the yzix node types
  [ `Dump`, `UnDump`, `Fetch`, `Require` ] (should be performed by evaluator+store).
* TODO: figure out how to do forced rebuilds (e.g. also prefer a node which is not
  the build node of the previous realisation of that task)

## coarse per-derivation workflow

```
    derivation
    |        |
    |        |
   key     build
    |        |
    |        |
    V        V
  inhash  outhash
    |     (either CA or IHA)
     \      /
      \    /
       \  /
    realisation
```

## build results

Just for completeness, two build results are currently considered:

* success: the build succeeded, and the result is uploaded to the central store
* failure: the build failed (e.g. build process terminated via error exit code or was killed)
* another case might be "partial": the build succeeded, but uploading to the
  central store failed (the result is only available on the build node that built it).
  This case is interesting, because we don't need to rerun the build, just the upload step
  needs to be fixed/done semi-manually (e.g. maybe the central store ran out of storage,
  or the network was unavailable)

## build task queue

It is naïve to think that something like a queue via `rabbitmq` (`AMQP`) or `MQTT`
suffices, because some requirements are missing:

1. some way to push build results to the clients, and these should be associated
  to the build inputs (a hacky way might use multiple queues for that, e.g.
  a `tasks` input queue and a `done` output queue).
2. some way to lock "inhashes" (see section inhash-exclusive).

The second point is somewhat easy to realise using `etcd`, and using the `watch`
mechanism it can be used to simulate a queue, and the inhash-addressing of
queued derivations can be seamlessly integrated.

TODO: maybe we want to adjust the priorities of tasks in the queue, but Nix currently
doesn't seem to do this, so consider this only when it starts to make sense as a
performance or lag optimization.
