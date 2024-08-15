# SPDX-FileCopyrightText: ☭ Emery Hemingway
# SPDX-License-Identifier: Unlicense

import
  std/[unittest],
  pkg/balls,
  pkg/preserves,
  ../src/nix_actor/[nix_api, nix_values, protocol]

suite "libexpr":
  initLibexpr()

  let
    store = openStore()
    state = newState(store, [])

  proc checkConversion(s: string) =
    var nixVal = state.evalFromString(s, "")
    state.force(nixVal)
    nixVal.close()
    var pr = state.toPreserves(nixVal)
    pr = pr.unthunkAll
    echo pr

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

  test "derivation":
    let samples = [
        "let pkgs = import <nixpkgs> { }; in pkgs.hello"
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

test "fromPreserve":
  const raw = "{ca: <bind <_>> ca-method: |fixed:r:sha256| deriver: <bind <_>> eris: #[CgA1VVrR0k5gjgU1wKQKVZr1RkANf4zUva3vyc2wmLzhzuL8XqeUL0HE4W3aRpXNwXyFbaLxtXJiLCUWSyLjej+h] name: \"default-builder.sh\" narHash: <bind <_>> narSize: <bind <_>> references: [] registrationTime: <bind <_>> sigs: <bind <_>> ultimate: <bind <_>>}"

  let pr = parsePreserves(raw)
  var attrs: AddToStoreClientAttrs
  check fromPreserve(attrs, pr)

suite "gatekeeper":

  test "nix-repo":
    var step: RepoResolveStep

    check not step.fromPreserves(parsePreserves"""
        <nix-store { import: "<nixpkgs>", lookupPath: ["nixpkgs=/home/repo/nixpkgs/channel"] }>
      """)

    check not step.fromPreserves(parsePreserves"""
        <nix-repo { }>
      """)

    check step.fromPreserves parsePreserves"""
        <nix-repo { import: "<nixpkgs>", lookupPath: ["nixpkgs=/home/repo/nixpkgs/channel"] }>
      """

    check step.fromPreserves parsePreserves"""
        <nix-repo { args: #f, import: "<nixpkgs>", lookupPath: ["nixpkgs=/home/repo/nixpkgs/channel"] }>
      """

    check step.fromPreserves parsePreserves"""
        <nix-repo { args: #f, import: "<nixpkgs>", lookupPath: ["nixpkgs=/home/repo/nixpkgs/channel"], store: "local" }>
      """
