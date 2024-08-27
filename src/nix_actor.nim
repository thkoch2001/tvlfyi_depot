# SPDX-FileCopyrightText: ☭ Emery Hemingway
# SPDX-License-Identifier: Unlicense

import
  std/[options, strutils, tables],
  pkg/preserves,
  pkg/preserves/sugar,
  pkg/syndicate,
  pkg/syndicate/[gatekeepers, patterns, relays],
  ./nix_actor/[nix_api, nix_values],
  ./nix_actor/protocol

proc echo(args: varargs[string, `$`]) {.used.} =
  stderr.writeLine(args)

type Value = preserves.Value

template tryPublish(turn: Turn, cap: Cap; body: untyped) =
  try: body
  except CatchableError as err:
    when not defined(release):
      stderr.writeLine err.msg
    publish(turn, cap, Error(message: %err.msg))

proc publishOk(turn: Turn; cap: Cap, v: Value) =
  publish(turn, cap, ResultOk(value: v))

proc publishError(turn: Turn; cap: Cap, v: Value) =
  publish(turn, cap, Error(message: v))

proc unembedEntity(emb: EmbeddedRef; E: typedesc): Option[E] =
  if emb of Cap and emb.Cap.target of E:
    result = emb.Cap.target.E.some

proc unembedEntity(v: Value; E: typedesc): Option[E] =
  if v.isEmbeddedRef:
    result = v.embeddedRef.unembedEntity(E)

proc openStore(uri: string; params: Option[AttrSet]): Store =
  var pairs: seq[string]
  if params.isSome:
    var  i: int
    pairs.setLen(params.get.len)
    for (key, val) in params.get.pairs:
      pairs[i] = $key & "=" & $val
      inc i
  openStore(uri, pairs)

type
  NixState = object
    store: Store
    eval: EvalState

proc initState(detail: NixResolveDetail): NixState =
  result.store = openStore(detail.storeUri.get("auto"), detail.storeParams)
  if detail.lookupPath.isSome:
    result.eval = newState(result.store, detail.lookupPath.get)
  else:
    result.eval = newState(result.store)

proc close(state: NixState) =
  if not state.eval.isNil:
    state.eval.close()
  if not state.store.isNil:
    state.store.close()

type
  NixEntity {.acyclic, final.} = ref object of Entity
    state: NixState
    self: Cap
    root: NixValue

proc newNixEntity(turn: Turn; detail: NixResolveDetail): NixEntity =
  ## Create an initial evaluation state.
  let entity = NixEntity(state: initState(detail))
  entity.root = entity.state.eval.initNull()
  turn.onStop do (turn: Turn):
    decref(entity.root)
    entity.state.close()
  entity.self = newCap(turn, entity)
  entity

proc newChild(parent: NixEntity; turn: Turn; val: NixValue): NixEntity =
  ## Create a child entity for a given root value.
  let entity = NixEntity(state: parent.state, root: val)
  turn.inFacet do (turn: Turn):
    entity.facet = turn.facet
    entity.self = newCap(turn, entity)
    turn.onStop do (turn: Turn):
      decref(entity.root)
  entity

proc serve(entity: NixEntity; turn: Turn; obs: Observe) =
  ## Dataspace emulation.
  let facet = turn.facet
  var
    analysis = analyse(obs.pattern)
    captures = newSeq[Value](analysis.capturePaths.len)
  block stepping:
    for i, path in analysis.constPaths:
      var v = entity.state.eval.step(entity.root, path)
      if v.isNone or v.get != analysis.constValues[i]:
        let null = initRecord("null")
        for v in captures.mitems: v = null
        break stepping
    for i, path in analysis.capturePaths:
      var v = entity.state.eval.step(entity.root, path)
      if v.isSome:
        captures[i] = v.get.unthunkAll
      else:
        captures[i] = initRecord("null")
  publish(turn, Cap obs.observer, captures)

proc serve(entity: NixEntity; turn: Turn; r: RealiseString) =
  tryPublish(turn, r.result.Cap):
    var str = entity.state.eval.realiseString(entity.root)
    publishOk(turn, r.result.Cap, %str)

proc serve(entity: NixEntity; turn: Turn; e: Eval) =
  tryPublish(turn, e.result.Cap):
    var expr = entity.state.eval.evalFromString(e.expr)
    expr = entity.state.eval.apply(expr, entity.root)
    expr = entity.state.eval.apply(expr, e.args.toNix(entity.state.eval))
    publishOk(turn, e.result.Cap, entity.newChild(turn, expr).self.toPreserves)

method publish(entity: NixEntity; turn: Turn; a: AssertionRef; h: Handle) =
  var
    # TODO: this would be a union object
    # but orc doesn't support it yet.
    eval: Eval
    observe: Observe
    realise: RealiseString
  if observe.fromPreserves(a.value):
    entity.serve(turn, observe)
  elif observe.fromPreserves(a.value) and observe.observer of Cap:
    serve(entity, turn, observe)
  elif realise.fromPreserves(a.value) and realise.result of Cap:
    serve(entity, turn, realise)
  elif eval.fromPreserves(a.value) and eval.result of Cap:
    serve(entity, turn, eval)
  else:
    when not defined(release):
      echo "unhandled assertion ", a.value

proc bootActor*(turn: Turn; relay: Cap) =
  initLibstore()
  initLibexpr()

  let gk = spawnGatekeeper(turn, relay)
  gk.serve do (turn: Turn; step: NixResolveStep) -> Resolved:
    newNixEntity(turn, step.detail).self.resolveAccepted

when isMainModule:
  runActor("main") do (turn: Turn):
    resolveEnvironment(turn, bootActor)
