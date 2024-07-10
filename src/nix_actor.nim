# SPDX-FileCopyrightText: ☭ Emery Hemingway
# SPDX-License-Identifier: Unlicense

import
  std/[options, os, osproc, streams, strtabs, strutils, tables, times],
  pkg/preserves, pkg/preserves/sugar,
  pkg/syndicate,
  pkg/syndicate/protocols/gatekeeper,
  pkg/syndicate/[patterns, relays],
  ./nix_actor/[nix_api, nix_api_expr, nix_api_store, nix_values, utils],
  ./nix_actor/protocol

proc echo(args: varargs[string, `$`]) {.used.} =
  stderr.writeLine(args)

type
  Value = preserves.Value
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
    for i, path in analysis.constPaths:
      var v = repo.state.step(repo.root, path)
      if v.isNone or v.get != analysis.constValues[i]:
        return
    for i, path in analysis.capturePaths:
      var v = repo.state.step(repo.root, path)
      if v.isSome:
        captures[i] = v.get.unthunkAll
    discard publish(turn, Cap obs.observer, captures)

proc main() =
  initLibstore()
  initLibexpr()

  runActor("main") do (turn: Turn):
    resolveEnvironment(turn) do (turn: Turn; relay: Cap):

      let resolvePat = Resolve?:{ 0: RepoResolveStep.grabWithin, 1: grab() }
      during(turn, relay, resolvePat) do (detail: RepoResolveDetail; observer: Cap):
        linkActor(turn, "nix-repo") do (turn: Turn):
          let repo = newRepoEntity(turn, detail)
          discard publish(turn, observer, ResolvedAccepted(responderSession: repo.self))

main()
