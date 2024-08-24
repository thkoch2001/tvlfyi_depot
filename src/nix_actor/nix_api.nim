# SPDX-FileCopyrightText: ☭ Emery Hemingway
# SPDX-License-Identifier: Unlicense

import
  ./nix_api_expr,
  ./nix_api_store,
  ./nix_api_types,
  ./nix_api_value,
  ./utils

export NixContext, Store, EvalState, Value, ValueType,
  gc_decref, isNil

type
  StringCallback = proc (s: string) {.closure.}
  StringCallbackState = object
    callback: StringCallback

proc receiveString(start: cstring; n: cuint; state: pointer) {.cdecl.} =
  let state = cast[ptr StringCallbackState](state)
  assert not state.isNil
  var buf = newString(n)
  if n > 0:
    copyMem(buf[0].addr, start, buf.len)
  state.callback(buf)

proc initLibstore*() =
  mitNix:
    discard nix.libstore_init()

proc initLibexpr*() =
  mitNix:
    discard nix.libexpr_init()

proc openStore*(uri = "auto", params: openarray[string] = []): Store =
  mitNix:
    if params.len == 0:
      result = nix.store_open(uri, nil)
    else:
      var args = allocCStringArray(params)
      defer: deallocCStringArray(args)
      result = nix.store_open(uri, addr args)
  assert not result.isNil

proc close*(store: Store) = store_free(store)

proc getUri*(store: Store; cb: StringCallback) =
  mitNix:
    let state = new StringCallbackState
    state.callback = cb
    discard nix.store_get_uri(store, receiveString, state[].addr)

proc getVersion*(store: Store; cb: StringCallback) =
  mitNix:
    let state = new StringCallbackState
    state.callback = cb
    discard nix.store_get_version(store, receiveString, state[].addr)

proc isValidPath*(store: Store; path: string): bool =
  assert not store.isNil
  assert path != ""
  mitNix:
    assert not nix.isNil
    let sp = nix.store_parse_path(store, path)
    if sp.isNil:
      raise newException(CatchableError, "store_parse_path failed")
    defer: store_path_free(sp)
    result = nix.store_is_valid_path(store, sp)

proc copyClosure*(src, dst: Store; path: string) =
  assert path != ""
  mitNix:
    let sp = nix.store_parse_path(src, path)
    if sp.isNil:
      raise newException(CatchableError, "store_parse_path failed")
    defer: store_path_free(sp)
    nix.store_copy_closure(src, dst, sp)

proc newState*(store: Store; lookupPath: openarray[string]): EvalState =
  mitNix:
    var path = allocCStringArray(lookupPath)
    defer: deallocCStringArray(path)
    result = nix.state_create(path, store)
  assert not result.isNil

proc close*(state: EvalState) = state_free(state)

proc close*(value: Value) =
  mitNix:
    discard nix.gc_decref(cast[pointer](value))

proc evalFromString*(nix: NixContext; state: EvalState; expr, path: string; result: Value)  =
  discard nix.expr_eval_from_string(state, expr, path, result)

proc evalFromString*(state: EvalState; expr: string; path = ""): Value =
  mitNix:
    try:
      result = nix.alloc_value(state)
      nix.evalFromString(state, expr, path, result)
    except CatchableError as err:
      result.close()
      raise err

proc force*(state: EvalState; value: Value) =
  mitNix:
    discard nix.value_force(state, value)

proc get_attr_byidx*(value: Value; state: EvalState; i: cuint): (cstring, Value) =
  mitNix:
    result[1] = nix.get_attr_byidx(value, state, i, addr result[0])

proc apply(nix: NixContext; state: EvalState; fn, arg: Value): Value =
  result = nix.alloc_value(state)
  discard nix.init_apply(result, fn, arg)

proc apply*(state: EvalState; fn, arg: Value): Value =
  mitNix:
    result = nix.apply(state, fn, arg)

proc call*(state: EvalState; fn: Value; args: varargs[Value]): Value =
  mitNix:
    result = nix.alloc_value(state)
    var array = cast[ptr UncheckedArray[Value]](args)
    discard nix.value_call_multi(state, fn, args.len.csize_t, array, result)
