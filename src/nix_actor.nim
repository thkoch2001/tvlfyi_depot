# SPDX-FileCopyrightText: ☭ Emery Hemingway
# SPDX-License-Identifier: Unlicense

import std/[os, strutils, tables]
import preserves, syndicate, syndicate/relays
# from syndicate/protocols/dataspace import Observe
import ./nix_actor/[nix_api, nix_api_value]
import ./nix_actor/protocol

proc toPreserve(state: State; value: Value; E = void): Preserve[E] {.gcsafe.} =
  var ctx: NixContext
  stderr.writeLine get_type(ctx, value).int
  case get_type(ctx, value)
  of NIX_TYPE_THUNK: raiseAssert "cannot preserve thunk"
  of NIX_TYPE_INT:
    result = getInt(ctx, value).toPreserve(E)
  of NIX_TYPE_FLOAT:
    result = getFloat(ctx, value).toPreserve(E)
  of NIX_TYPE_BOOL:
    result = getBool(ctx, value).toPreserve(E)
  of NIX_TYPE_STRING:
    result = ($getString(ctx, value)).toPreserve(E)
  of NIX_TYPE_PATH:
    result = ($getPathString(ctx, value)).toPreserve(E)
  of NIX_TYPE_NULL:
    result = initRecord[E]("null")
  of NIX_TYPE_ATTRS:
    result = initDictionary(E)
    let n = getAttrsSize(ctx, value)
    var i: cuint
    while i < n:
      var (key, val) = get_attr_byidx(ctx, value, state, i)
      inc(i)
      result[toSymbol($key, E)] = toPreserve(state, val, E)
      stderr.writeLine(result)
      # close(val)
  of NIX_TYPE_LIST:
    let n = getListSize(ctx, value)
    result = initSequence(n, E)
    var i: cuint
    while i < n:
      var val = getListByIdx(ctx, value, state, i)
      result[i] = toPreserve(state, val, E)
      inc(i)
      # close(val)
  of NIX_TYPE_FUNCTION, NIX_TYPE_EXTERNAL:
    raiseAssert "TODO: need a failure type"

type
  BootArgs {.preservesDictionary.} = object
    dataspace: Cap

proc main() =
  initLibexpr()

  runActor("nix_actor") do (root: Cap; turn: var Turn):
    connectStdio(turn, root)

    during(turn, root, ?BootArgs) do (ds: Cap):
      let
        store = openStore()
        state = newState(store)

      let pat = ?Observe(pattern: !Eval) ?? {0: grabLit(), 1: grabLit()}
      during(turn, ds, pat) do (expr: string, path: string):
        var
          value: Value
          ass = Eval(expr: expr, path: path)
        try:
          value = evalFromString(state, ass.expr, ass.path)
          force(state, value)
          ass.result = toPreserve(state, value, void)
          discard publish(turn, ds, ass)
        except CatchableError as err:
          stderr.writeLine "failed to evaluate ", ass.expr, ": ", err.msg
        close(value)
    do:
      close(state)
      close(store)

main()
