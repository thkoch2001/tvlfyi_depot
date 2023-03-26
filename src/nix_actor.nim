# SPDX-FileCopyrightText: ☭ Emery Hemingway
# SPDX-License-Identifier: Unlicense

import std/[asyncdispatch, json, osproc]
import preserves, preserves/jsonhooks
import syndicate
from syndicate/protocols/dataspace import Observe
import ./nix_actor/protocol
import ./nix_actor/[main, store]

type Observe = dataspace.Observe[Ref]

proc build(spec: string): Build =
  var execOutput = execProcess("nix", args = ["build", "--json", "--no-link", spec], options = {poUsePath})
  stderr.writeLine execOutput
  var js = parseJson(execOutput)
  Build(input: spec, output: js[0].toPreserve)

proc bootNixFacet(ds: Ref; turn: var Turn): Facet =
  # let store = openStore()
  inFacet(turn) do (turn: var Turn):
    let storePathObservation = ?Observe(pattern: !Build) ?? {0: grabLit()}
    during(turn, ds, storePathObservation) do (spec: string):
      stderr.writeLine "build ", spec
      let a = build(spec)
      discard publish(turn, ds, a)

proc bootNixActor(root: Ref; turn: var Turn) =
  connectStdio(root, turn)
  during(turn, root, ?Serve) do (ds: Ref):
    discard bootNixFacet(ds, turn)

initNix() # Nix lib isn't actually being used but it's nice to know that it links.
bootDataspace("main", bootNixActor)
runForever()
