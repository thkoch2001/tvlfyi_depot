# SPDX-FileCopyrightText: ☭ Emery Hemingway
# SPDX-License-Identifier: Unlicense

import
  std/[options, os, osproc, streams, strtabs, strutils, tables, times],
  pkg/preserves, pkg/preserves/sugar,
  pkg/syndicate,
  pkg/syndicate/protocols/gatekeeper,
  pkg/syndicate/relays,
  ./nix_actor/[nix_api, nix_api_expr, nix_api_store, nix_values, utils],
  ./nix_actor/protocol

proc echo(args: varargs[string, `$`]) {.used.} =
  stderr.writeLine(args)

type
  Value = preserves.Value

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

proc realiseCallback(userdata: pointer; a, b: cstring) {.cdecl.} =
  echo "realiseCallback(nil, ", a, ", ", b, ")"

proc realise(turn: Turn; store: Store; path: string; resp: Cap) =
  mitNix:
    let storePath = nix.store_parse_path(store, path)
    assert not storePath.isNil
    defer: store_path_free(storePath)
    turn.facet.run do (turn: Turn):
      discard nix.store_realise(store, storePath, nil, realiseCallback)
      if nix.store_is_valid_path(store, storePath):
        discard publish(turn, resp, initRecord("ok", %path))
      else:
        discard publish(turn, resp, initRecord("error", %"not a valid store path"))

proc eval(store: Store; state: EvalState; expr: string): EvalResult =
  defer: close(state) # Single-use state.
  var nixVal: NixValue
  try:
    nixVal = state.evalFromString(expr, "")
    state.force(nixVal)
    result = EvalResult(orKind: EvalResultKind.ok)
    result.ok.result = nixVal.toPreserves(state).unthunkAll
    result.ok.expr = expr
  except CatchableError as err:
    reset result
    result.err.message = err.msg

proc evalFile(store: Store; state: EvalState; path: string; prArgs: Value): EvalFileResult =
  defer: close(state) # Single-use state.
  var
    fn, arg, res: NixValue
  try:
    arg = prArgs.toNix(state)
    fn = state.evalFromString("import " & path, "")
    res = apply(state, fn, arg)
    state.force(res)
    result = EvalFileResult(orKind: EvalFileResultKind.ok)
    result.ok.result = res.toPreserves(state).unthunkAll
    result.ok.args = prArgs
    result.ok.path = path
  except CatchableError as err:
    reset result
    result.err.message = err.msg

proc serve(turn: Turn; detail: ResolveDetail; store: Store; ds: Cap) =
  during(turn, ds, Eval.grabWithin) do (expr: string, resp: Cap):
    let state = newState(store, detail.lookupPath)
    discard publish(turn, resp, eval(store, state, expr))

  during(turn, ds, EvalFile.grabWithin) do (path: string, args: Value, resp: Cap):
    let state = newState(store, detail.lookupPath)
    discard publish(turn, resp, evalFile(store, state, path, args))

  during(turn, ds, Realise.grabWithin) do (drv: string, resp: Cap):
    realise(turn, store, drv, resp)

proc main() =
  initLibstore()
  initLibexpr()

  runActor("main") do (turn: Turn):
    resolveEnvironment(turn) do (turn: Turn; relay: Cap):
      let pat = Resolve?:{ 0: ResolveStep.grabWithin, 1: grab() }
      during(turn, relay, pat) do (detail: ResolveDetail; observer: Cap):
        let
          store = openStore(detail.`store-uri`)
          ds = turn.newDataspace()
        linkActor(turn, "nix-actor") do (turn: Turn):
          serve(turn, detail, store, ds)
        discard publish(turn, observer, ResolvedAccepted(responderSession: ds))
      do:
        close(store)

main()
