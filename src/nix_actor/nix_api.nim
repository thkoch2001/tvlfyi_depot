# SPDX-FileCopyrightText: ☭ Emery Hemingway
# SPDX-License-Identifier: Unlicense

import ./nix_api_types, ./nix_api_value, ./nix_api_store, ./nix_api_expr

export NixContext, Store, EvalState, Value, ValueType,
  gc_decref, isNil

{.passC: staticExec"$PKG_CONFIG --cflags nix-expr-c".}
{.passL: staticExec"$PKG_CONFIG --libs nix-expr-c".}

# Always pass NixContext as a nil pointer.

proc initLibexpr* =
  var ctx: NixContext
  discard libexpr_init(ctx)

proc openStore*(uri: string, params: varargs[string]): Store =
  var ctx: NixContext
  var args = allocCStringArray(params)
  defer: deallocCStringArray(args)
  result = store_open(ctx, uri, addr args)

proc openStore*(): Store =
  var ctx: NixContext
  result = store_open(ctx, nil, nil)

proc close*(store: Store) = store_free(store)

proc newState*(store: Store; lookupPath: openarray[string]): EvalState =
  var ctx: NixContext
  var path = allocCStringArray(lookupPath)
  defer: deallocCStringArray(path)
  result = state_create(ctx, path, store)

proc close*(state: EvalState) = state_free(state)

proc newValue*(state: EvalState): Value =
  var ctx: NixContext
  alloc_value(ctx, state)

proc evalFromString*(state: EvalState; expr, path: string): Value =
  var ctx: NixContext
  result = alloc_value(ctx, state)
  discard expr_eval_from_string(ctx, state, expr, path, result)

proc close*(value: Value) =
  var ctx: NixContext
  discard gc_decref(ctx, cast[pointer](value))

proc force*(state: EvalState; value: Value) =
  var ctx: NixContext
  discard value_force(ctx, state, value)

proc get_attr_byidx*(ctx: NixContext; value: Value; state: EvalState; i: cuint): (cstring, Value) =
  var ctx: NixContext
  result[1] = get_attr_byidx(ctx, value, state, i, addr result[0])
