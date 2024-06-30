# SPDX-FileCopyrightText: ☭ Emery Hemingway
# SPDX-License-Identifier: Unlicense

import
  std/[options, os, osproc, streams, strtabs, strutils, tables, times],
  pkg/preserves,
  pkg/syndicate,
  pkg/syndicate/protocols/gatekeeper,
  pkg/syndicate/relays,
  ./nix_actor/[nix_api, nix_values],
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

proc realise(facet: Facet; detail: ResolveDetail; drv: string; log: Option[Cap], resp: Cap) =
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
    doAssert storePaths != @[]
    facet.run do (turn: Turn):
      for path in storePaths:
        discard publish(turn, resp, RealiseSuccess(
            storePath: path, drvPath: drv))
  except CatchableError as err:
    facet.run do (turn: Turn):
      discard publish(turn, resp, Error(message: err.msg))
  finally:
    close(p)

proc eval(store: Store; state: EvalState; expr: string): EvalResult =
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
    finally:
      close(nixVal)

proc evalFile(store: Store; state: EvalState; path: string; prArgs: Value): EvalFileResult =
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
    finally:
      close(res)
      close(arg)
      close(fn)

proc serve(turn: Turn; detail: ResolveDetail; store: Store; state: EvalState; ds: Cap) =
  during(turn, ds, Eval.grabWithin) do (expr: string, resp: Cap):
    discard publish(turn, resp, eval(store, state, expr))

  during(turn, ds, EvalFile.grabWithin) do (path: string, args: Value, resp: Cap):
    discard publish(turn, resp, evalFile(store, state, path, args))

  during(turn, ds, Realise.grabWithin) do (drv: string, log: Value, resp: Cap):
    realise(turn.facet, detail, drv, log.unembed(Cap), resp)

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
