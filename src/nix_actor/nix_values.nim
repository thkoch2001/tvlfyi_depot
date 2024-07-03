# SPDX-FileCopyrightText: ☭ Emery Hemingway
# SPDX-License-Identifier: Unlicense

import
  std/options,
  pkg/preserves,
  ./[nix_api, nix_api_util, nix_api_value, utils]

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
  mitNix:
      # TODO: use a context for error handling
    let kind = nix.get_type(value)
    case kind
    of NIX_TYPE_THUNK: raiseAssert "cannot preserve thunk"
    of NIX_TYPE_INT:
      result = nix.getInt(value).toPreserves
    of NIX_TYPE_FLOAT:
      result = nix.getFloat(value).toPreserves
    of NIX_TYPE_BOOL:
      result = nix.getBool(value).toPreserves
    of NIX_TYPE_STRING:
      let thunk = StringThunkRef()
      let err = nix.getString(value, thunkString, thunk[].addr)
      doAssert err == NIX_OK, $err
      result = thunk.embed
    of NIX_TYPE_PATH:
      result = ($nix.getPathString(value)).toPreserves
    of NIX_TYPE_NULL:
      result = initRecord("null")
    of NIX_TYPE_ATTRS:
      if nix.has_attr_byname(value, state, "drvPath"):
        result = initRecord("drv",
            nix.get_attr_byname(value, state, "drvPath").toPreserves(state),
            nix.get_attr_byname(value, state, "outPath").toPreserves(state),
          )
      else:
        let n = nix.getAttrsSize(value)
        result = initDictionary(int n)
        var i: cuint
        while i < n:
          let (key, val) = get_attr_byidx(value, state, i)
          result[($key).toSymbol] = val.toPreserves(state)
          inc(i)
    of NIX_TYPE_LIST:
      let n = nix.getListSize(value)
      result = initSequence(n)
      var i: cuint
      while i < n:
        var val = nix.getListByIdx(value, state, i)
        result[i] = val.toPreserves(state)
        inc(i)
    of NIX_TYPE_FUNCTION:
      result = "«function»".toPreserves
    of NIX_TYPE_EXTERNAL:
      result = "«external»".toPreserves

proc translate*(nix: NixContext; state: EvalState; pr: preserves.Value): NixValue =
  try:
    result = nix.alloc_value(state)
    case pr.kind
    of pkBoolean:
      nix.init_bool(result, pr.bool)
    of pkFloat:
      nix.init_float(result, pr.float.cdouble)
    of pkRegister:
      nix.init_int(result, pr.register.int64)
    of pkBigInt:
      nix.init_int(result, pr.register.int64)
    of pkString:
      nix.init_string(result, pr.string)
    of pkByteString:
      raise newException(ValueError, "cannot convert large Preserves integer to Nix: " & $pr)
    of pkSymbol:
      nix.evalFromString(state, cast[string](pr.symbol), "", result)
    of pkRecord:
      if pr.isRecord("null", 0):
        nix.init_null(result)
      elif pr.isRecord("drv", 2):
        let b = nix.make_bindings_builder(state, 2)
        defer: bindings_builder_free(b)
        nix.bindings_builder_insert(b, "drvPath", nix.translate(state, pr.fields[0]))
        nix.bindings_builder_insert(b, "outPath", nix.translate(state, pr.fields[1]))
        nix.make_attrs(result, b)
      else:
        raise newException(ValueError, "cannot convert Preserves record to Nix: " & $pr)
    of pkSequence, pkSet:
      let b = nix.make_list_builder(state, pr.len.csize_t)
      defer: list_builder_free(b)
      for i, e in pr:
        discard nix.list_builder_insert(b, i.register.cuint, nix.translate(state, e))
      nix.make_list(b, result)
    of pkDictionary:
      let b = nix.make_bindings_builder(state, pr.dict.len.csize_t)
      defer: bindings_builder_free(b)
      for (name, value) in pr.dict:
        if name.isSymbol:
          nix.bindings_builder_insert(b, name.symbol.string, nix.translate(state, value))
        else:
          nix.bindings_builder_insert(b, $name, nix.translate(state, value))
      nix.make_attrs(result, b)
    of pkEmbedded:
      raise newException(ValueError, "cannot convert Preserves embedded value to Nix")
  except CatchableError as err:
    result.close()
    raise err

proc toNix*(pr: preserves.Value; state: EvalState): NixValue =
  mitNix:
    result = nix.translate(state, pr)
