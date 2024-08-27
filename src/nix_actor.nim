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

proc serve(entity: NixEntity; turn: Turn; checkPath: CheckStorePath) =
  tryPublish(turn, checkPath.valid.Cap):
    let v = entity.state.store.isValidPath(checkPath.path)
    publish(turn, checkPath.valid.Cap, initRecord("ok", %v))

proc serve(entity: NixEntity; turn: Turn; copy: CopyClosure) =
  var dest = copy.dest.unembedEntity(NixEntity)
  if dest.isNone:
    publishError(turn, copy.result.Cap, %"destination store is not colocated with source store")
  else:
    tryPublish(turn, copy.result.Cap):
      entity.state.store.copyClosure(dest.get.state.store, copy.storePath)
      publishOk(turn, copy.result.Cap, %true)
        # TODO: assert some stats or something.

proc serve(entity: NixEntity; turn: Turn; obs: Observe) =
  let facet = turn.facet
  #[
  # TODO: move to a store entity
  if obs.pattern.matches(initRecord("uri", %"")):
    entity.state.store.getUri do (s: string):
      facet.run do (turn: Turn):
        publish(turn, obs.observer.Cap, obs.pattern.capture(initRecord("uri", %s)).get)
  elif obs.pattern.matches(initRecord("version", %"")):
    entity.state.store.getVersion do (s: string):
      facet.run do (turn: Turn):
        publish(turn, obs.observer.Cap, obs.pattern.capture(initRecord("version", %s)).get)
  else:
  ]#
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
        captures[i] = turn.facet.exportNix(v.get)
      else:
        captures[i] = initRecord("null")
  publish(turn, Cap obs.observer, captures)

proc serve(entity: NixEntity; turn: Turn; r: RealiseString) =
  tryPublish(turn, r.result.Cap):
    publishOk(turn, r.result.Cap,
        %entity.state.eval.realiseString(entity.root))

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
    checkPath: CheckStorePath
    copyClosure: CopyClosure
    eval: Eval
    observe: Observe
    realise: RealiseString

  if checkPath.fromPreserves(a.value):
    entity.serve(turn, checkPath)
  elif observe.fromPreserves(a.value):
    entity.serve(turn, observe)
  elif copyClosure.fromPreserves(a.value) and
      copyClosure.result of Cap:
    entity.serve(turn, copyClosure)
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
