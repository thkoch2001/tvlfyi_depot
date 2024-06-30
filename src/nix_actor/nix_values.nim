# SPDX-FileCopyrightText: ☭ Emery Hemingway
# SPDX-License-Identifier: Unlicense

import
  std/options,
  pkg/preserves,
  ./[nix_api, nix_api_util, nix_api_value]

proc echo(args: varargs[string, `$`]) {.used.} =
  stderr.writeLine(args)

type
  Value = preserves.Value
  NixValue* = nix_api.Value
  StringThunkRef = ref StringThunkObj
  StringThunkObj = object of EmbeddedObj
    data: Option[string]

proc thunkString(start: cstring; n: cuint; state: pointer) {.cdecl.} =
  let thunk = cast[ptr StringThunkObj](state)
  assert thunk.data.isNone
  var buf = newString(n)
  if n > 0:
    copyMem(buf[0].addr, start, buf.len)
  thunk.data = buf.move.some

proc unthunk*(v: Value): Value =
  let thunk = v.unembed(StringThunkRef)
  assert thunk.isSome
  assert thunk.get.data.isSome
  thunk.get.data.get.toPreserves

proc unthunkAll*(v: Value): Value =
  v.mapEmbeds(unthunk)

proc toPreserves*(value: NixValue; state: EvalState): Value {.gcsafe.} =
  var ctx: NixContext # nil
    # TODO: use a context for error handling
  let kind = get_type(ctx, value)
  case kind
  of NIX_TYPE_THUNK: raiseAssert "cannot preserve thunk"
  of NIX_TYPE_INT:
    result = getInt(ctx, value).toPreserves
  of NIX_TYPE_FLOAT:
    result = getFloat(ctx, value).toPreserves
  of NIX_TYPE_BOOL:
    result = getBool(ctx, value).toPreserves
  of NIX_TYPE_STRING:
    let thunk = StringThunkRef()
    let err = getString(ctx, value, thunkString, thunk[].addr)
    doAssert err == NIX_OK, $err
    result = thunk.embed
  of NIX_TYPE_PATH:
    result = ($getPathString(ctx, value)).toPreserves
  of NIX_TYPE_NULL:
    result = initRecord("null")
  of NIX_TYPE_ATTRS:
    if has_attr_byname(ctx, value, state, "drvPath"):
      result = initRecord("drv",
          get_attr_byname(ctx, value, state, "drvPath").toPreserves(state),
          get_attr_byname(ctx, value, state, "outPath").toPreserves(state),
        )
    else:
      let n = getAttrsSize(ctx, value)
      result = initDictionary(int n)
      var i: cuint
      while i < n:
        let (key, val) = get_attr_byidx(ctx, value, state, i)
        result[($key).toSymbol] = val.toPreserves(state)
        inc(i)
  of NIX_TYPE_LIST:
    let n = getListSize(ctx, value)
    result = initSequence(n)
    var i: cuint
    while i < n:
      var val = getListByIdx(ctx, value, state, i)
      result[i] = val.toPreserves(state)
      inc(i)
  of NIX_TYPE_FUNCTION:
    result = "«function»".toPreserves
  of NIX_TYPE_EXTERNAL:
    result = "«external»".toPreserves

proc translate*(ctx: NixContext; state: EvalState; pr: preserves.Value): NixValue =
  try:
    result = alloc_value(ctx, state)
    case pr.kind
    of pkBoolean:
      ctx.check init_bool(ctx, result, pr.bool)
    of pkFloat:
      ctx.check init_float(ctx, result, pr.float.cdouble)
    of pkRegister:
      ctx.check init_int(ctx, result, pr.register.int64)
    of pkBigInt:
      ctx.check init_int(ctx, result, pr.register.int64)
    of pkString:
      ctx.check init_string(ctx, result, pr.string)
    of pkByteString:
      raise newException(ValueError, "cannot convert large Preserves integer to Nix: " & $pr)
    of pkSymbol:
      evalFromString(ctx, state, cast[string](pr.symbol), "", result)
    of pkRecord:
      if pr.isRecord("null", 0):
        ctx.check init_null(ctx, result)
      elif pr.isRecord("drv", 2):
        let b = make_bindings_builder(ctx, state, 2)
        defer: bindings_builder_free(b)
        ctx.check bindings_builder_insert(ctx, b, "drvPath", translate(ctx, state, pr.fields[0]))
        ctx.check bindings_builder_insert(ctx, b, "outPath", translate(ctx, state, pr.fields[1]))
        ctx.check make_attrs(ctx, result, b)
      else:
        raise newException(ValueError, "cannot convert Preserves record to Nix: " & $pr)
    of pkSequence, pkSet:
      let b = make_list_builder(ctx, state, pr.len.csize_t)
      defer: list_builder_free(b)
      for i, e in pr:
        ctx.check list_builder_insert(ctx, b, i.register.cuint, translate(ctx, state, e))
      ctx.check make_list(ctx, b, result)
    of pkDictionary:
      let b = make_bindings_builder(ctx, state, pr.dict.len.csize_t)
      defer: bindings_builder_free(b)
      for (name, value) in pr.dict:
        if name.isSymbol:
          ctx.check bindings_builder_insert(ctx, b, name.symbol.string, translate(ctx, state, value))
        else:
          ctx.check bindings_builder_insert(ctx, b, $name, translate(ctx, state, value))
      ctx.check make_attrs(ctx, result, b)
    of pkEmbedded:
      raise newException(ValueError, "cannot convert Preserves embedded value to Nix")
  except CatchableError as err:
    result.close()
    raise err

proc toNix*(pr: preserves.Value; state: EvalState): NixValue =
  let ctx = c_context_create()
  defer: c_context_free(ctx)
  result = translate(ctx, state, pr)
