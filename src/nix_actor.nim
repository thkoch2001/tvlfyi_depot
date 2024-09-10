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
  publish(turn, cap, protocol.ResultOk(value: v))

proc publishError(turn: Turn; cap: Cap, v: Value) =
  publish(turn, cap, protocol.Error(message: v))

proc fromEmbedded[E](entity: var E; emb: EmbeddedRef): bool =
  if emb of Cap and emb.Cap.target of E:
    entity = emb.Cap.target.E
    result = true

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
  NixEntity = ref object of Entity
    self: Cap
    store: Store

  StoreEntity {.final.} = ref object of NixEntity

  EvalEntity {.final.} = ref object of NixEntity
    state: EvalState
    root: NixValue

proc newStoreEntity(turn: Turn; detail: StoreResolveDetail): StoreEntity =
  let entity = StoreEntity(store: openStore(detail.storeUri, detail.storeParams))
  entity.self = turn.newCap(entity)
  entity.self.relay.onStop do (turn: Turn):
    entity.store.close()
  entity

proc serve(entity: StoreEntity; turn: Turn; checkPath: CheckStorePath) =
  tryPublish(turn, checkPath.valid.Cap):
    var v = entity.store.isValidPath(checkPath.path)
    publish(turn, checkPath.valid.Cap, initRecord("ok", %v))

proc serve(entity: NixEntity; turn: Turn; rep: Replicate) =
  tryPublish(turn, rep.result.Cap):
    var
      target: Store
      otherEntity = rep.target.unembedEntity(NixEntity)
    if otherEntity.isSome:
      target = otherEntity.get.store
    if target.isNil:
      publishError(turn, rep.result.Cap, %"cannot replicate with target")
    else:
      if entity.store.isValidPath(rep.storePath):
        entity.store.copyClosure(target, rep.storePath)
          # path exists at entity
      else:
        target.copyClosure(entity.store, rep.storePath)
          # path hopefully exists at target
      publishOk(turn, rep.result.Cap, %rep.storePath)

method publish(entity: StoreEntity; turn: Turn; a: AssertionRef; h: Handle) =
  var
    # TODO: this would be a union object
    # but orc doesn't support it yet.
    checkStorePath: CheckStorePath
    replicate: Replicate
  if checkStorePath.fromPreserves(a.value):
    entity.serve(turn, checkStorePath)
  elif replicate.fromPreserves(a.value):
    entity.serve(turn, replicate)

proc newEvalEntity(turn: Turn; detail: EvalResolveDetail): EvalEntity =
  ## Create an initial evaluation state.
  let entity = EvalEntity(
      store: openStore(detail.storeUri.get("auto"), detail.storeParams)
    )
  if detail.lookupPath.isSome:
    entity.state = newState(entity.store, detail.lookupPath.get)
  else:
    entity.state = newState(entity.store)
  entity.root = entity.state.initNull()
  entity.self = turn.newCap(entity)
  entity.self.relay.onStop do (turn: Turn):
    decref(entity.root)
    entity.state.close()
    entity.store.close()
  entity

proc newChild(parent: EvalEntity; turn: Turn; val: NixValue): EvalEntity =
  ## Create a child entity for a given root value.
  let entity = EvalEntity(
      store: parent.store,
      state: parent.state,
      root: val
    )
  turn.inFacet do (turn: Turn):
    entity.facet = turn.facet
    entity.self = newCap(turn, entity)
    entity.self.relay.onStop do (turn: Turn):
      decref(entity.root)
  entity

proc serve(entity: EvalEntity; turn: Turn; obs: Observe) =
  ## Dataspace emulation.
  let facet = turn.facet
  var
    analysis = analyse(obs.pattern)
    captures = newSeq[Value](analysis.capturePaths.len)
  block stepping:
    for i, path in analysis.constPaths:
      var v = entity.state.step(entity.root, path)
      if v.isNone or v.get != analysis.constValues[i]:
        let null = initRecord("null")
        for v in captures.mitems: v = null
        break stepping
    for i, path in analysis.capturePaths:
      var v = entity.state.step(entity.root, path)
      if v.isSome:
        captures[i] = v.get.unthunkAll
      else:
        captures[i] = initRecord("null")
  publish(turn, Cap obs.observer, captures)

proc serve(entity: EvalEntity; turn: Turn; r: RealiseString) =
  tryPublish(turn, r.result.Cap):
    var str = entity.state.realiseString(entity.root)
    publishOk(turn, r.result.Cap, %str)

proc serve(entity: EvalEntity; turn: Turn; e: Eval) =
  tryPublish(turn, e.result.Cap):
    var expr = entity.state.evalFromString(e.expr)
    expr = entity.state.apply(expr, entity.root)
    expr = entity.state.apply(expr, e.args.toNix(entity.state))
    publishOk(turn, e.result.Cap, entity.newChild(turn, expr).self.toPreserves)

method publish(entity: EvalEntity; turn: Turn; a: AssertionRef; h: Handle) =
  var
    # TODO: this would be a union object
    # but orc doesn't support it yet.
    eval: Eval
    observe: Observe
    realise: RealiseString
    replicate: Replicate
  if observe.fromPreserves(a.value) and observe.observer of Cap:
    serve(entity, turn, observe)
  elif realise.fromPreserves(a.value) and realise.result of Cap:
    serve(entity, turn, realise)
  elif eval.fromPreserves(a.value) and eval.result of Cap:
    serve(entity, turn, eval)
  elif replicate.fromPreserves(a.value) and replicate.result of Cap:
    serve(entity, turn, replicate)
  else:
    when not defined(release):
      echo "unhandled assertion ", a.value

proc bootActor*(turn: Turn; relay: Cap) =
  initLibstore()
  initLibexpr()

  let gk = spawnGatekeeper(turn, relay)

  gk.serve do (turn: Turn; step: StoreResolveStep) -> rpc.Result:
    newStoreEntity(turn, step.detail).self.resultOk

  gk.serve do (turn: Turn; step: EvalResolveStep) -> rpc.Result:
    newEvalEntity(turn, step.detail).self.resultOk

when isMainModule:
  runActor("main") do (turn: Turn):
    resolveEnvironment(turn, bootActor)
