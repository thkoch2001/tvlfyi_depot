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
    publish(turn, cap, Error(message: %err.msg))

proc publishOk(turn: Turn; cap: Cap, v: Value) =
  publish(turn, cap, ResultOk(value: v))

proc publishError(turn: Turn; cap: Cap, v: Value) =
  publish(turn, cap, Error(message: v))

proc unembedEntity(emb: EmbeddedRef; E: typedesc): Option[E] =
  if emb of Cap and emb.Cap.target of E:
    result = emb.Cap.target.E.some

type
  StoreEntity {.final.} = ref object of Entity
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
  tryPublish(turn, checkPath.valid.Cap):
    let v = entity.store.isValidPath(checkPath.path)
    publish(turn, checkPath.valid.Cap, initRecord("ok", %v))

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

method serve(entity: StoreEntity; turn: Turn; copy: CopyClosure) =
  var dest = copy.dest.unembedEntity(StoreEntity)
  if dest.isNone:
    publishError(turn, copy.result.Cap, %"destination store is not colocated with source store")
  else:
    tryPublish(turn, copy.result.Cap):
      entity.store.copyClosure(dest.get.store, copy.storePath)
      publishOk(turn, copy.result.Cap, %true)
        # TODO: assert some stats or something.

method publish(entity: StoreEntity; turn: Turn; a: AssertionRef; h: Handle) =
  var
    # orc doesn't handle this as a union object
    observe: Observe
    checkPath: CheckStorePath
    copyClosure: CopyClosure
  if checkPath.fromPreserves(a.value):
    entity.serve(turn, checkPath)
  elif observe.fromPreserves(a.value):
    entity.serve(turn, observe)
  elif copyClosure.fromPreserves(a.value) and
      copyClosure.result of Cap:
    entity.serve(turn, copyClosure)
  else:
    when not defined(release):
      echo "nix-store: unhandled assertion ", a.value

type
  RepoEntity {.final.} = ref object of Entity
    self: Cap
    store: StoreEntity
    state: EvalState
    root: NixValue

proc newRepoEntity(turn: Turn; detail: RepoResolveDetail): RepoEntity =
  let entity = RepoEntity()
  turn.onStop do (turn: Turn):
    if not entity.state.isNil:
      entity.state.close()
  if detail.store.isSome:
    var other = detail.store.get.unembed(StoreEntity)
    if other.isSome:
      entity.store = other.get
    elif detail.store.get.isString:
      var storeDetail = StoreResolveDetail(cache: detail.cache, uri: detail.store.get.string)
      entity.store = newStoreEntity(turn, storeDetail)
    else:
      raise newException(CatchableError, "invalid store parameter for nix-repo: " & $detail.store.get)
  else:
    var storeDetail = StoreResolveDetail(cache: detail.cache, uri: "auto")
    entity.store = newStoreEntity(turn, storeDetail)
  entity.state = newState(entity.store.store, detail.lookupPath)
  entity.root = entity.state.evalFromString("import " & detail.`import`, "")
  if detail.args.isSome:
    var na = detail.args.get.toNix(entity.state)
    entity.root = entity.state.apply(entity.root, na)
  entity.self = newCap(turn, entity)
  entity

proc serve(repo: RepoEntity; turn: Turn; obs: Observe) =
  var
    analysis = analyse(obs.pattern)
    captures = newSeq[Value](analysis.capturePaths.len)
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
        captures[i] = turn.facet.exportNix(v.get)
      else: captures[i] = initRecord("null")
  discard publish(turn, Cap obs.observer, captures)

proc serve(repo: RepoEntity; turn: Turn; r: Realise) =
  tryPublish(turn, r.result.Cap):
    var drv: Derivation
    if not drv.fromPreserves(r.value):
      publishError(turn, r.result.Cap, %("failed to parse Derivation: " & $r.value))
    else:
      var dummyCap = drv.context.unembed(Cap)
      if dummyCap.isNone:
        publishError(turn, r.result.Cap, %"derivation context is not a Cap")
      else:
        if not(dummyCap.get.target of NixValueRef):
          publishError(turn, r.result.Cap, %"derivation context is not a NixValueRef")
        else:
          let v = repo.state.realise(dummyCap.get.target.NixValueRef.value)
          publishOk(turn, r.result.Cap, v)
            # TODO: this is awkward.

method publish(repo: RepoEntity; turn: Turn; a: AssertionRef; h: Handle) =
  ## Respond to observations with dataspace semantics, minus retraction
  ## of assertions in response to the retraction of observations.
  ## This entity is scoped to immutable data so this shouldn't be a problem.
  var
    obs: Observe
    realise: Realise
  if obs.fromPreserves(a.value) and obs.observer of Cap:
    serve(repo, turn, obs)
  elif realise.fromPreserves(a.value) and realise.result of Cap:
    serve(repo, turn, realise)
  else:
    when not defined(release):
      echo "nix-repo: unhandled assertion ", a.value

proc main() =
  initLibstore()
  initLibexpr()

  runActor("main") do (turn: Turn):
    resolveEnvironment(turn) do (turn: Turn; relay: Cap):
      let gk = spawnGatekeeper(turn, relay)

      gk.serve do (turn: Turn; step: StoreResolveStep) -> Resolved:
        newStoreEntity(turn, step.detail).self.resolveAccepted

      gk.serve do (turn: Turn; step: RepoResolveStep) -> Resolved:
        newRepoEntity(turn, step.detail).self.resolveAccepted

main()
