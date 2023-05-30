# SPDX-FileCopyrightText: ☭ Emery Hemingway
# SPDX-License-Identifier: Unlicense

import std/[asyncdispatch, json, osproc, strutils, tables]
import preserves, preserves/jsonhooks
import syndicate
from syndicate/protocols/dataspace import Observe
import ./nix_actor/protocol
import ./nix_actor/[main, store]

type
  Value = Preserve[void]
  Options = Table[Symbol, Value]
  Observe = dataspace.Observe[Ref]

proc build(spec: string): Build =
  var execOutput = execProcess("nix", args = ["build", "--json", "--no-link", spec], options = {poUsePath})
  var js = parseJson(execOutput)
  Build(input: spec, output: js[0].toPreserve)

proc realise(realise: Realise): seq[string] =
  var execlines = execProcess("nix-store", args = ["--realize", realise.drv], options = {poUsePath})
  split(strip(execlines), '\n')

proc eval(eval: Eval): Value =
  var args = @["eval", "--json", "--expr", eval.expr]
  for sym, val in eval.options:
    add(args, "--" & $sym)
    if not val.isString "":
      var js: JsonNode
      if fromPreserve(js, val): add(args, $js)
      else: stderr.writeLine "invalid option ", sym, " ", val
  var execOutput = strip execProcess("nix", args = args, options = {poUsePath})
  if execOutput != "":
    var js = parseJson(execOutput)
    result = js.toPreserve

proc bootNixFacet(ds: Ref; turn: var Turn): Facet =
  # let store = openStore()
  result = inFacet(turn) do (turn: var Turn):

    during(turn, ds, ?Observe(pattern: !Build) ?? {0: grabLit()}) do (spec: string):
      discard publish(turn, ds, build(spec))

    during(turn, ds, ?Observe(pattern: !Realise) ?? {0: grabLit()}) do (drvPath: string):
      var ass = Realise(drv: drvPath)
      ass.outputs = realise(ass)
      discard publish(turn, ds, ass)

    during(turn, ds, ?Observe(pattern: !Eval) ?? {0: grabLit(), 1: grabDict()}) do (e: string, o: Value):
      var ass = Eval(expr: e)
      if not fromPreserve(ass.options, unpackLiterals(o)):
        stderr.writeLine "invalid options ", o
      else:
        ass.result = eval(ass)
        discard publish(turn, ds, ass)

type Args {.preservesDictionary.} = object
  dataspace: Ref

proc bootNixActor(root: Ref; turn: var Turn) =
  connectStdio(root, turn)
  during(turn, root, ?Args) do (ds: Ref):
    discard bootNixFacet(ds, turn)

initNix() # Nix lib isn't actually being used but it's nice to know that it links.
runActor("main", bootNixActor)
