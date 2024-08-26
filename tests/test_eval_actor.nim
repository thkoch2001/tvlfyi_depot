# SPDX-FileCopyrightText: ☭ Emery Hemingway
# SPDX-License-Identifier: Unlicense

import
  std/options,
  pkg/balls,
  pkg/sys/ioqueue,
  pkg/preserves,
  pkg/preserves/sugar,
  pkg/syndicate,
  pkg/syndicate/protocols/[gatekeeper, rpc],
  ../src/nix_actor,
  ../src/nix_actor/[nix_api, nix_values, protocol]

type Value = preserves.Value

initLibstore()
initLibexpr()

type
  ResultContinuation {.final.} = ref object of Entity
    cb: proc (turn: Turn; v: Value)

method publish(cont: ResultContinuation; turn: Turn; ass: AssertionRef; h: Handle) =
  cont.cb(turn, ass.value)

proc newResultContinuation[T](turn: Turn; cb: proc (turn: Turn; v: T)): Cap =
  proc wrapper(turn: Turn; v: Value) =
    var
      err: ResultError
      ok: ResultOk
    if err.fromPreserves(v):
      raiseAssert $err.error
    check ok.fromPreserves(v)
    var x = ok.value.preservesTo(T)
    check x.isSome
    if x.isSome: cb(turn, x.get)
  turn.newCap(ResultContinuation(cb: wrapper))

suite "basic":

  var completed: bool

  proc actorTest(turn: Turn) =
    turn.onStop do (turn: Turn):
      block:
        ## actor stopped
        check completed

    checkpoint "actor booted"
    let rootFacet = turn.facet
    let ds = turn.newDataspace()

    let stepC = newResultContinuation(turn) do (turn: Turn; nix: Cap):
      checkpoint "stepC"
      block:
        ## stepC
        onPublish(turn, nix, grab()) do (v: Value):
          checkpoint("stepC grabbed nix value " & $v)
          assert not v.isRecord("null")
          assert v == %"Hello VolgaSprint!"
          completed = true
          stop(rootFacet)

    let stepB = newResultContinuation(turn) do (turn: Turn; nix: Cap):
      checkpoint "stepB"
      block:
        ## stepB
        onPublish(turn, nix, grab()) do (v: Value):
          checkpoint("stepB grabbed nix value " & $v)
          assert not v.isRecord("null")
          check v == %"Hello Volga"
        publish(turn, nix, Eval(
            expr: "y: x: x + y",
            args: %"Sprint!",
            result: stepC
          ))

    let stepA = newResultContinuation(turn) do (turn: Turn; nix: Cap):
      checkpoint "stepA"
      block:
        ## stepA
        onPublish(turn, nix, grab()) do (v: Value):
          checkpoint "stepA grabbed nix value " & $v
          assert not v.isRecord("null")
          check v == %"Hello"
        publish(turn, nix, Eval(
            expr: "y: x: x + y",
            args: %" Volga",
            result: stepB
          ))

    during(turn, ds, ResolvedAccepted.grabWithin) do (nix: Cap):
      checkpoint "resolve accepted"
      block:
        ## Resolved nix actor through gatekeeper
        onPublish(turn, nix, grab()) do (v: Value):
          checkpoint $v
        publish(turn, nix, Eval(
            expr: "y: x: y",
            args: %"Hello",
            result: stepA,
          ))

    during(turn, ds, Rejected.grabType) do (rej: Rejected):
      raiseAssert("resolve failed: " & $rej)

    publish(turn, ds, Resolve(
        step: parsePreserves"""<nix { }>""",
        observer: ds,
      ))

    nix_actor.bootActor(turn, ds)

  block:
    ## runActor
    runActor("tests", actorTest)
    check completed

suite "nixpkgs":

  var completed: bool

  proc actorTest(turn: Turn) =
    turn.onStop do (turn: Turn):
      block:
        ## actor stopped
        check completed

    checkpoint "actor booted"
    let rootFacet = turn.facet
    let ds = turn.newDataspace()

    let stepC = newResultContinuation(turn) do (turn: Turn; nix: Cap):
      checkpoint "stepC"
      block:
        ## stepC
        onPublish(turn, nix, grab()) do (v: Value):
          checkpoint("stepC grabbed nix value " & $v)
          assert v == %"https://9fans.github.io/plan9port/"
          completed = true
          stop(rootFacet)

    let stepB = newResultContinuation(turn) do (turn: Turn; nix: Cap):
      checkpoint "stepB"
      block:
        ## stepB
        publish(turn, nix, Eval(
            expr: "_: pkg: pkg.meta.homepage",
            args: %false,
            result: stepC
          ))

    let stepA = newResultContinuation(turn) do (turn: Turn; nix: Cap):
      checkpoint "stepA"
      block:
        ## stepA
        publish(turn, nix, Eval(
            expr: "builtins.getAttr",
            args: %"plan9port",
            result: stepB
          ))

    during(turn, ds, ResolvedAccepted.grabWithin) do (nix: Cap):
      checkpoint "resolve accepted"
      block:
        ## Resolved nix actor through gatekeeper
        onPublish(turn, nix, grab()) do (v: Value):
          checkpoint $v
        publish(turn, nix, Eval(
            expr: "args: _: import <nixpkgs> args",
            args: initDictionary(),
            result: stepA,
          ))

    during(turn, ds, Rejected.grabType) do (rej: Rejected):
      raiseAssert("resolve failed: " & $rej)

    publish(turn, ds, Resolve(
        step: parsePreserves"""
            <nix { lookupPath: [ "nixpkgs=/home/repo/nixpkgs" ] }>
          """,
        observer: ds,
      ))

    nix_actor.bootActor(turn, ds)

  block:
    ## runActor
    runActor("tests", actorTest)
    check completed
