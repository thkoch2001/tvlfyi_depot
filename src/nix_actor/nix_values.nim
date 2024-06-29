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
  var ctx: NixContext
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
