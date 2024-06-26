# SPDX-FileCopyrightText: ☭ Emery Hemingway
# SPDX-License-Identifier: Unlicense

import
  std/[options, os, osproc, streams, strtabs, strutils, tables, times],
  pkg/preserves,
  pkg/syndicate,
  pkg/syndicate/protocols/gatekeeper,
  pkg/syndicate/relays,
  ./nix_actor/[nix_api, nix_api_util, nix_api_value],
  ./nix_actor/protocol

proc echo(args: varargs[string, `$`]) {.used.} =
  stderr.writeLine(args)

type
  Value = preserves.Value
  NixValue = nix_api.Value
  StringThunkRef = ref StringThunkObj
  StringThunkObj = object of EmbeddedObj
    data: Option[string]

proc thunkString(start: cstring; n: cuint; state: pointer) {.cdecl.} =
  let thunk = cast[ptr StringThunkObj](state)
  assert thunk.data.isNone
  var buf = newString(n)
  copyMem(buf[0].addr, start, buf.len)
  thunk.data = buf.move.some

proc unthunk(v: Value): Value =
  let thunk = v.unembed(StringThunkRef)
  assert thunk.isSome
  assert thunk.get.data.isSome
  thunk.get.data.get.toPreserves

proc toPreserves(value: NixValue; state: EvalState): Value {.gcsafe.} =
  var ctx: NixContext
  case get_type(ctx, value)
  of NIX_TYPE_THUNK: raiseAssert "cannot preserve thunk"
  of NIX_TYPE_INT:
    result = getInt(ctx, value).toPreserves
  of NIX_TYPE_FLOAT:
    result = getFloat(ctx, value).toPreserves
  of NIX_TYPE_BOOL:
    result = getBool(ctx, value).toPreserves
  of NIX_TYPE_STRING:
    let thunk = StringThunkRef()
    let err = getString(ctx, value, thunkString, thunk[].addr)
    doAssert err == NIX_OK, $err
    result = thunk.embed
  of NIX_TYPE_PATH:
    result = ($getPathString(ctx, value)).toPreserves
  of NIX_TYPE_NULL:
    result = initRecord("null")
  of NIX_TYPE_ATTRS:
    result = initDictionary()
    let n = getAttrsSize(ctx, value)
    var i: cuint
    while i < n:
      var (key, val) = get_attr_byidx(ctx, value, state, i)
      inc(i)
      result[toSymbol($key)] = val.toPreserves(state)
      # close(val)
  of NIX_TYPE_LIST:
    let n = getListSize(ctx, value)
    result = initSequence(n)
    var i: cuint
    while i < n:
      var val = getListByIdx(ctx, value, state, i)
      result[i] = val.toPreserves(state)
      inc(i)
      # close(val)
  of NIX_TYPE_FUNCTION, NIX_TYPE_EXTERNAL:
    raiseAssert "TODO: need a failure type"

proc findCommand(detail: ResolveDetail; cmd: string): string =
  for dir in detail.`command-path`:
    result = dir / cmd
    if result.fileExists:
      return
  raise newException(OSError, "could not find " & cmd)

proc commandlineArgs(detail: ResolveDetail; args: varargs[string]): seq[string] =
  result = newSeqOfCap[string](detail.options.len * 2 + args.len)
  for sym, val in detail.options:
    result.add("--" & $sym)
    if not val.isString "":
      result.add(val.jsonText)
  for arg in args:
    result.add arg

proc commandlineEnv(detail: ResolveDetail): StringTableRef =
  newStringTable({"NIX_PATH": detail.lookupPath.join ":"})

proc instantiate(facet: Facet; detail: ResolveDetail; expr: string; log: Option[Cap]): InstantiateResult =
  # TODO: run this process asynchronously with nim-sys.
  var p: Process
  try:
    p = startProcess(
        detail.findCommand("nix-instantiate"),
        args = detail.commandlineArgs("--expr", expr),
        env = detail.commandlineEnv(),
        options = {},
      )
    var
      errors = errorStream(p)
      line = "".toPreserves
    while true:
      if errors.readLine(line.string):
        if log.isSome:
          facet.run do (turn: Turn):
            message(turn, log.get, line)
      elif not running(p): break
      initDuration(milliseconds = 250).some.runOnce
    var path = p.outputStream.readAll.strip
    if path != "":
      result = InstantiateResult(orKind: InstantiateResultKind.Derivation)
      result.derivation.expr = expr
      result.derivation.storePath = path
  except CatchableError as err:
    reset result
    result.error.message = err.msg
  finally:
    close(p)

proc realise(facet: Facet; detail: ResolveDetail; drv: string; log: Option[Cap]): RealiseResult =
  # TODO: run this process asynchronously with nim-sys.
  var p: Process
  try:
    p = startProcess(
        detail.findCommand("nix-store"),
        args = detail.commandlineArgs("--realise", drv),
        env = detail.commandlineEnv(),
        options = {},
      )
    var
      errors = errorStream(p)
      line = "".toPreserves
    while true:
      if errors.readLine(line.string):
        if log.isSome:
          facet.run do (turn: Turn):
            message(turn, log.get, line)
      elif not running(p): break
      initDuration(milliseconds = 250).some.runOnce
    var storePaths = p.outputStream.readAll.strip.split
    if storePaths != @[]:
      result = RealiseResult(orKind: RealiseResultKind.Outputs)
      result.outputs.drv = drv
      result.outputs.storePaths = storePaths
  except CatchableError as err:
    reset result
    result.error.message = err.msg
  finally:
    close(p)

proc eval(store: Store; state: EvalState; expr: string): EvalResult =
    var nixVal: NixValue
    try:
      nixVal = state.evalFromString(expr, "")
      state.force(nixVal)
      result = EvalResult(orKind: EvalResultKind.EvalSuccess)
      result.evalsuccess.expr = expr
      result.evalsuccess.result = nixVal.toPreserves(state).mapEmbeds(unthunk)
    except CatchableError as err:
      reset result
      result.error.message = err.msg
    finally:
      close(nixVal)

proc evalFile(store: Store; state: EvalState; path: string; args: Value): EvalFileResult =
    var
      js = args.jsonText
    stderr.writeLine "converted ", $args, " to ", js
    var
      expr = """import $1 (builtins.fromJSON ''$2'')""" % [ path, js ]
        # TODO: convert to NixValue instead of using JSON conversion.
      nixVal: NixValue
    try:
      nixVal = state.evalFromString(expr, "")
      state.force(nixVal)
      result = EvalFileResult(orKind: EvalFileResultKind.success)
      result.success.path = path
      result.success.args = args
      result.success.result = nixVal.toPreserves(state).mapEmbeds(unthunk)
    except CatchableError as err:
      reset result
      result.error.message = err.msg
    finally:
      close(nixVal)

proc serve(turn: Turn; detail: ResolveDetail; store: Store; state: EvalState; ds: Cap) =
  during(turn, ds, Eval.grabWithin) do (expr: string, resp: Cap):
    discard publish(turn, resp, eval(store, state, expr))

  during(turn, ds, EvalFile.grabWithin) do (path: string, args: Value, resp: Cap):
    discard publish(turn, resp, evalFile(store, state, path, args))

  during(turn, ds, Instantiate.grabWithin) do (expr: string, log: Value, resp: Cap):
    discard publish(turn, resp, instantiate(turn.facet, detail, expr, log.unembed(Cap)))

  during(turn, ds, Realise.grabWithin) do (drv: string, log: Value, resp: Cap):
    discard publish(turn, resp, realise(turn.facet, detail, drv, log.unembed(Cap)))

proc main() =
  initLibexpr()

  runActor("main") do (turn: Turn):
    resolveEnvironment(turn) do (turn: Turn; relay: Cap):
      let pat = Resolve?:{ 0: ResolveStep.grabWithin, 1: grab() }
      during(turn, relay, pat) do (detail: ResolveDetail; observer: Cap):
        let
          store = openStore()
          state = newState(store, detail.lookupPath)
          ds = turn.newDataspace()
            # TODO: attenuate this dataspace to only the assertions we observe.
        linkActor(turn, "nix-actor") do (turn: Turn):
          serve(turn, detail, store, state, ds)
        discard publish(turn, observer, ResolvedAccepted(responderSession: ds))
      do:
        close(state)
        close(store)

main()
