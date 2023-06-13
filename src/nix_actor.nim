# SPDX-FileCopyrightText: ☭ Emery Hemingway
# SPDX-License-Identifier: Unlicense

import std/[json, os, osproc, parseutils, strutils, tables]
import eris/memory_stores
import preserves, preserves/jsonhooks
import syndicate
from syndicate/protocols/dataspace import Observe
import ./nix_actor/[clients, daemons]
import ./nix_actor/libnix/[libexpr, main, store]
import ./nix_actor/protocol

type
  Value = Preserve[void]
  Observe = dataspace.Observe[Ref]

proc toPreserve(val: libexpr.ValueObj | libExpr.ValuePtr; state: EvalState; E = void): Preserve[E] {.gcsafe.} =
  ## Convert a Nix value to a Preserves value.
  case val.kind
  of nThunk:
    result = initRecord[E]("thunk")
  of nInt:
    result = val.integer.toPreserve(E)
  of nFloat:
    result = val.fpoint.toPreserve(E)
  of nBool:
    result = val.boolean.toPreserve(E)
  of nString:
    result = val.shallowString.toPreserve(E)
  of nPath:
    result = toSymbol($val.path, E)
  of nNull:
    result = initRecord[E]("null")
  of nAttrs:
    result = initDictionary(E)
    for key, val in val.pairs:
      result[symbolString(state, key).toSymbol(E)] = val.toPreserve(state, E)
  of nList:
    result = initSequence(0, E)
    for e in val.items:
      result.sequence.add(e.toPreserve(state, E))
  of nFunction:
    result = initRecord[E]("func")
  of nExternal:
    result = initRecord[E]("external")

proc eval(state: EvalState; code: string): Value =
  ## Evaluate Nix `code` to a Preserves value.
  var nixVal: libexpr.ValueObj
  let expr = state.parseExprFromString(code, getCurrentDir())
  state.eval(expr, nixVal)
  state.forceValueDeep(nixVal)
  nixVal.toPreserve(state, void)

proc parseArgs(args: var seq[string]; opts: AttrSet) =
  for sym, val in opts:
    add(args, "--" & $sym)
    if not val.isString "":
      var js: JsonNode
      if fromPreserve(js, val): add(args, $js)
      else: stderr.writeLine "invalid option --", sym, " ", val

#[
proc parseNarinfo(info: var AttrSet; text: string) =
  var
    key, val: string
    off: int
  while off < len(text):
    off = off + parseUntil(text, key, ':', off) + 1
    off = off + skipWhitespace(text, off)
    off = off + parseUntil(text, val, '\n', off) + 1
    if key != "" and val != "":
      if allCharsInSet(val, Digits):
        info[Symbol key] = val.parsePreserves
      else:
        info[Symbol key] = val.toPreserve

proc narinfo(turn: var Turn; ds: Ref; path: string) =
  let
    client = newAsyncHttpClient()
    url = "https://cache.nixos.org/" & path & ".narinfo"
    futGet = get(client, url)
  addCallback(futGet, turn) do (turn: var Turn):
    let resp = read(futGet)
    if code(resp) != Http200:
      close(client)
    else:
      let futBody = body(resp)
      addCallback(futBody, turn) do (turn: var Turn):
        close(client)
        var narinfo = Narinfo(path: path)
        parseNarinfo(narinfo.info, read(futBody))
        discard publish(turn, ds, narinfo)
]# # I never link to openssl if I can avoid it.

proc build(spec: string): Build =
  var execOutput = execProcess("nix", args = ["build", "--json", "--no-link", spec], options = {poUsePath})
  var js = parseJson(execOutput)
  Build(input: spec, output: js[0].toPreserve)

proc realise(realise: Realise): seq[string] =
  var execlines = execProcess("nix-store", args = ["--realize", realise.drv], options = {poUsePath})
  split(strip(execlines), '\n')

proc instantiate(instantiate: Instantiate): Value =
  const cmd = "nix-instantiate"
  var args = @["--expr", instantiate.expr]
  parseArgs(args, instantiate.options)
  var execOutput = strip execProcess(cmd, args = args, options = {poUsePath})
  execOutput.toPreserve

proc bootNixFacet(turn: var Turn; ds: Ref): Facet =
  # let store = openStore()
  result = inFacet(turn) do (turn: var Turn):

    during(turn, ds, ?Observe(pattern: !Build) ?? {0: grabLit()}) do (spec: string):
      discard publish(turn, ds, build(spec))

    during(turn, ds, ?Observe(pattern: !Realise) ?? {0: grabLit()}) do (drvPath: string):
      var ass = Realise(drv: drvPath)
      ass.outputs = realise(ass)
      discard publish(turn, ds, ass)

    during(turn, ds, ?Observe(pattern: !Instantiate) ?? {0: grabLit(), 1: grabDict()}) do (e: string, o: Value):
      var ass = Instantiate(expr: e)
      if not fromPreserve(ass.options, unpackLiterals(o)):
        stderr.writeLine "invalid options ", o
      else:
        ass.result = instantiate(ass)
        discard publish(turn, ds, ass)

    #[
    during(turn, ds, ?Observe(pattern: !Narinfo) ?? {0: grabLit()}) do (path: string):
      narinfo(turn, ds, path)
      ]#

type
  RefArgs {.preservesDictionary.} = object
    dataspace: Ref
  ClientSideArgs {.preservesDictionary.} = object
    `listen-socket`: string
  DaemonSideArgs {.preservesDictionary.} = object
    `daemon-socket`: string

main.initNix()
libexpr.initGC()

runActor("main") do (root: Ref; turn: var Turn):
  let
    erisStore = newMemoryStore()
    nixStore = openStore()
    nixState = newEvalState(nixStore)
  connectStdio(root, turn)

  during(turn, root, ?RefArgs) do (ds: Ref):
    discard bootNixFacet(turn, ds)

    during(turn, ds, ?Observe(pattern: !Eval) ?? {0: grabLit(), 1: grabDict()}) do (e: string, o: Assertion):
      var ass = Eval(expr: e)
      doAssert fromPreserve(ass.options, unpackLiterals(o))
        # unused options
      ass.result = eval(nixState, ass.expr)
      discard publish(turn, ds, ass)

    during(turn, root, ?ClientSideArgs) do (socketPath: string):
      bootClientSide(turn, ds, erisStore, socketPath)

    during(turn, root, ?DaemonSideArgs) do (socketPath: string):
      bootDaemonSide(turn, ds, erisStore, socketPath)
