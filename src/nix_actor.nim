# SPDX-FileCopyrightText: ☭ Emery Hemingway
# SPDX-License-Identifier: Unlicense

import
  std/[options, os, osproc, streams, strtabs, strutils, tables, times],
  pkg/preserves,
  pkg/preserves/sugar,
  pkg/syndicate,
  pkg/syndicate/protocols/gatekeeper,
  pkg/syndicate/[patterns, relays],
  ./nix_actor/[nix_api, nix_api_expr, nix_api_store, nix_values, utils],
  ./nix_actor/protocol

proc echo(args: varargs[string, `$`]) {.used.} =
  stderr.writeLine(args)

type Value = preserves.Value

type
  RepoEntity = ref object of Entity
    self: Cap
    store: Store
    state: EvalState
    root: NixValue

proc newRepoEntity(turn: Turn; detail: RepoResolveDetail): RepoEntity =
  let entity = RepoEntity()
  turn.onStop do (turn: Turn):
    if not entity.state.isNil:
      entity.state.close()
    if not entity.store.isNil:
      entity.store.close()
  entity.store = openStore(detail.store)
  entity.state = newState(entity.store, detail.lookupPath)
  entity.root = entity.state.evalFromString("import " & detail.`import`, "")
  if detail.args.isSome:
    var na = detail.args.get.toNix(entity.state)
    entity.root = entity.state.apply(entity.root, na)
  entity.self = newCap(turn, entity)
  entity

method publish(repo: RepoEntity; turn: Turn; a: AssertionRef; h: Handle) =
  ## Respond to observations with dataspace semantics, minus retraction
  ## of assertions in response to the retraction of observations.
  ## This entity is scoped to immutable data so this shouldn't be a problem.
  var obs: Observe
  if obs.fromPreserves(a.value) and obs.observer of Cap:
    var analysis = analyse(obs.pattern)
    var captures = newSeq[Value](analysis.capturePaths.len)
    block stepping:
      for i, path in analysis.constPaths:
        var v = repo.state.step(repo.root, path)
        if v.isNone or v.get != analysis.constValues[i]:
          let null = initRecord("null")
          for v in captures.mitems: v = null
          break stepping
      for i, path in analysis.capturePaths:
        var v = repo.state.step(repo.root, path)
        if v.isSome:
          captures[i] = v.get.unthunkAll
        else: captures[i] = initRecord("null")
    discard publish(turn, Cap obs.observer, captures)

type
  StoreEntity = ref object of Entity
    self: Cap
    store: Store

proc openStore(uri: string; params: AttrSet): Store =
  var
    pairs = newSeq[string](params.len)
    i: int
  for (key, val) in params.pairs:
    pairs[i] = $key & "=" & $val
    inc i
  openStore(uri, pairs)

proc newStoreEntity(turn: Turn; detail: StoreResolveDetail): StoreEntity =
  let entity = StoreEntity()
  turn.onStop do (turn: Turn):
    if not entity.store.isNil:
      entity.store.close()
      reset entity.store
  entity.store = openStore(detail.uri, detail.params)
  entity.self = newCap(turn, entity)
  entity

proc serve(entity: StoreEntity; turn: Turn; checkPath: CheckStorePath) =
  try:
    let v = entity.store.isValidPath(checkPath.path)
    publish(turn, checkPath.valid.Cap, initRecord("ok", %v))
  except CatchableError as err:
    publish(turn, checkPath.valid.Cap, initRecord("error", %err.msg))

proc serve(entity: StoreEntity; turn: Turn; obs: Observe) =
  let facet = turn.facet
  if obs.pattern.matches(initRecord("uri", %"")):
    entity.store.getUri do (s: string):
      facet.run do (turn: Turn):
        publish(turn, obs.observer.Cap, obs.pattern.capture(initRecord("uri", %s)).get)
  if obs.pattern.matches(initRecord("version", %"")):
    entity.store.getVersion do (s: string):
      facet.run do (turn: Turn):
        publish(turn, obs.observer.Cap, obs.pattern.capture(initRecord("version", %s)).get)

method publish(entity: StoreEntity; turn: Turn; a: AssertionRef; h: Handle) =
  var
    # orc doesn't handle this as a union object
    observe: Observe
    checkPath: CheckStorePath
  if checkPath.fromPreserves(a.value):
    entity.serve(turn, checkPath)
  elif observe.fromPreserves(a.value):
    entity.serve(turn, observe)
  else:
    echo "unhandled assertion ", a.value

proc main() =
  initLibstore()
  initLibexpr()

  runActor("main") do (turn: Turn):
    resolveEnvironment(turn) do (turn: Turn; relay: Cap):

      let resolveRepoPat = Resolve?:{ 0: RepoResolveStep.grabWithin, 1: grab() }
      during(turn, relay, resolveRepoPat) do (detail: RepoResolveDetail; observer: Cap):
        linkActor(turn, "nix-repo") do (turn: Turn):
          let repo = newRepoEntity(turn, detail)
          discard publish(turn, observer, ResolvedAccepted(responderSession: repo.self))

      let resolveStorePat = Resolve?:{ 0: StoreResolveStep.grabWithin, 1: grab() }
      during(turn, relay, resolveStorePat) do (detail: StoreResolveDetail; observer: Cap):
        linkActor(turn, "nix-store") do (turn: Turn):
          let e = newStoreEntity(turn, detail)
          discard publish(turn, observer, ResolvedAccepted(responderSession: e.self))

main()
