# SPDX-FileCopyrightText: ☭ Emery Hemingway
# SPDX-License-Identifier: Unlicense

import std/[asyncdispatch, httpclient, json, osproc, parseutils, strutils, tables]
import preserves, preserves/jsonhooks
import syndicate
from syndicate/protocols/dataspace import Observe
import ./nix_actor/protocol
import ./nix_actor/[main, sockets]

type
  Value = Preserve[void]
  Observe = dataspace.Observe[Ref]

proc parseArgs(args: var seq[string]; opts: Dict) =
  for sym, val in opts:
    add(args, "--" & $sym)
    if not val.isString "":
      var js: JsonNode
      if fromPreserve(js, val): add(args, $js)
      else: stderr.writeLine "invalid option --", sym, " ", val

proc parseNarinfo(info: var Dict; text: string) =
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

proc eval(eval: Eval): Value =
  const cmd = "nix"
  var args = @["eval", "--expr", eval.expr]
  parseArgs(args, eval.options)
  var execOutput = strip execProcess(cmd, args = args, options = {poUsePath})
  if execOutput != "":
    var js = parseJson(execOutput)
    result = js.toPreserve

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

    during(turn, ds, ?Observe(pattern: !Eval) ?? {0: grabLit(), 1: grabDict()}) do (e: string, o: Value):
      var ass = Eval(expr: e)
      if not fromPreserve(ass.options, unpackLiterals(o)):
        stderr.writeLine "invalid options ", o
      else:
        ass.result = eval(ass)
        discard publish(turn, ds, ass)

    during(turn, ds, ?Observe(pattern: !Narinfo) ?? {0: grabLit()}) do (path: string):
      narinfo(turn, ds, path)

type
  RefArgs {.preservesDictionary.} = object
    dataspace: Ref
  ClientSideArgs {.preservesDictionary.} = object
    `listen-socket`: string
  DaemonSideArgs {.preservesDictionary.} = object
    `daemon-socket`: string

proc bootNixActor(root: Ref; turn: var Turn) =
  connectStdio(root, turn)

  during(turn, root, ?RefArgs) do (ds: Ref):
    discard bootNixFacet(turn, ds)

    during(turn, root, ?ClientSideArgs) do (socketPath: string):
      bootClientSide(turn.facet, ds, socketPath)

    during(turn, root, ?DaemonSideArgs) do (socketPath: string):
      bootDaemonSide(turn, ds, socketPath)

initNix() # Nix lib isn't actually being used but it's nice to know that it links.
runActor("main", bootNixActor)
