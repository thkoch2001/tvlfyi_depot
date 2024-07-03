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

{.passC: staticExec"$PKG_CONFIG --cflags nix-expr-c".}
{.passL: staticExec"$PKG_CONFIG --libs nix-expr-c".}

proc initLibexpr*() =
  mitNix:
    discard nix.libexpr_init()

proc openStore*(uri: string, params: varargs[string]): Store =
  mitNix:
    var args = allocCStringArray(params)
    defer: deallocCStringArray(args)
    result = nix.store_open(uri, addr args)

proc openStore*(): Store =
  mitNix:
    result = nix.store_open(nil, nil)

proc close*(store: Store) = store_free(store)

proc newState*(store: Store; lookupPath: openarray[string]): EvalState =
  mitNix:
    var path = allocCStringArray(lookupPath)
    defer: deallocCStringArray(path)
    result = nix.state_create(path, store)

proc close*(state: EvalState) = state_free(state)

proc close*(value: Value) =
  mitNix:
    discard nix.gc_decref(cast[pointer](value))

proc evalFromString*(nix: NixContext; state: EvalState; expr, path: string; result: Value)  =
  discard nix.expr_eval_from_string(state, expr, path, result)

proc evalFromString*(state: EvalState; expr, path: string): Value =
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
