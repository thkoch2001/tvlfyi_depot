# SPDX-FileCopyrightText: ☭ Emery Hemingway
# SPDX-License-Identifier: Unlicense

import
  std/options,
  pkg/balls,
  pkg/sys/ioqueue,
  pkg/preserves,
  pkg/preserves/sugar,
  pkg/syndicate,
  ../src/nix_actor/[nix_api, nix_values, nix_api_value, nix_api_expr]

type Value = preserves.Value

initLibstore()
initLibexpr()

suite "libexpr":

  let
    store = openStore()
    state = newState(store, ["nixpkgs"])

  proc checkConversion(s: string) =
    runActor("checkConversion") do (turn: Turn):
      var nixVal = state.evalFromString(s, "")
      state.force(nixVal)
      nixVal.close()
      var pr = nixVal.toPreserves(state)
      checkpoint $pr
      var wirePr = pr.unthunkAll
      checkpoint $wirePr

  test "lists":
    let samples = [
        "[]",
        "[null]",
        "[[]]",
        "[ null [ null [null null null null null null null null ] null ] null ]",
      ]
    for s in samples:
      test s:
        checkConversion(s)

  test "attrsets":
    let samples = [
        "{}",
        "{a = {}; }",
        "{a = { x = {}; }; b = null; c = null; d = null; e = null; }",
      ]
    for s in samples:
      test s:
        checkConversion(s)

  test "large":
    let samples =
      "builtins.listToAttrs (builtins.genList (x: { name = toString x; value = null; }) 99)"
    checkConversion(samples)

type AddToStoreClientAttrs {.preservesDictionary.} = object
  ## A subset of AddToStoreAttrs
  `ca-method`: Symbol
  eris: seq[byte]
  name: string

suite "fromPreserve":
  const raw = "{ca: <bind <_>> ca-method: |fixed:r:sha256| deriver: <bind <_>> eris: #[CgA1VVrR0k5gjgU1wKQKVZr1RkANf4zUva3vyc2wmLzhzuL8XqeUL0HE4W3aRpXNwXyFbaLxtXJiLCUWSyLjej+h] name: \"default-builder.sh\" narHash: <bind <_>> narSize: <bind <_>> references: [] registrationTime: <bind <_>> sigs: <bind <_>> ultimate: <bind <_>>}"

  let pr = parsePreserves(raw)
  var attrs: AddToStoreClientAttrs
  check fromPreserve(attrs, pr)

suite "eval":
  let
    store = openStore()
    eval = store.newState()

  test "function":
    let
      fn = eval.evalFromString("x: y: x + y")
      x = (%"foo").toNix(eval)
      y = (%"bar").toNix(eval)
    checkpoint "fn:", fn.typeName
    checkpoint " x:", x.typeName
    checkpoint " y:", y.typeName
    let
      r = eval.apply(eval.apply(fn, x), y)
      pr = r.toPreserves(eval).unthunkAll

    checkpoint $pr
    check $pr == """"foobar""""

  eval.close()
  store.close()
