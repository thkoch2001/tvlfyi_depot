# SPDX-FileCopyrightText: ☭ Emery Hemingway
# SPDX-License-Identifier: Unlicense

import ./stdpuspus, ./store

{.passC: staticExec"$PKG_CONFIG --cflags nix-expr".}
{.passL: staticExec"$PKG_CONFIG --libs nix-expr".}

proc parentDir(path: string): string =
  var i = path.high
  while path[i] != '/': dec(i)
  path[0..i]

{.passC: "-I" & parentDir(currentSourcePath).}

type
  NixInt* = int64
  NixFloat* = float64

  ValueKind* {.importcpp: "nix::ValueType", header: "value.hh".} = enum
    nThunk,
    nInt,
    nFloat,
    nBool,
    nString,
    nPath,
    nNull,
    nAttrs,
    nList,
    nFunction,
    nExternal,
  Value* = ValueObj | ValuePtr
  ValuePtr* = ptr ValueObj
  ValueObj* {.importcpp: "nix::Value", header: "value.hh".} = object
    integer*: NixInt
    boolean*: bool
    string: StringContext
    path*: cstring
    fpoint*: NixFloat
    attrs: Bindings
  StringContext = object
    s: cstring
  Symbol* {.importcpp: "nix::Symbol", header: "symbol-table.hh".} = object
    discard
  Attr {.importcpp: "nix::Attr", header: "attr-set.hh".} = object
    name: Symbol
    value: ValuePtr
  Bindings = ptr BindginsObj
  BindginsObj {.importcpp: "nix::Bindings", header: "attr-set.hh".} = object
    discard

proc kind*(val: Value): ValueKind {.importcpp: "#.type()".}

proc showType*(val: Value): StdString {.importcpp.}

proc shallowString*(val: Value): string =
  if val.kind != nString:
    raise newException(FieldDefect, "Value not an attribute set")
  $val.string.s

proc size(bindings: Bindings): csize_t {.importcpp.}

proc `[]`(b: Bindings; i: Natural): Attr {.importcpp: "(*#)[#]".}

iterator pairs*(val: Value): (Symbol, ValuePtr) =
  if val.kind != nAttrs:
    raise newException(FieldDefect, "Value not an attribute set")
  for i in 0..<val.attrs.size():
    let attr = val.attrs[i]
    yield (attr.name, attr.value)

proc listSize(val: Value): csize_t {.importcpp.}

proc listElems(val: Value): ptr UncheckedArray[ValuePtr] {.importcpp.}

iterator items*(val: Value): ValuePtr =
  if val.kind != nList:
    raise newException(FieldDefect, "Value not a list")
  for i in 0..<val.listSize:
    yield val.listElems()[i]

type
  ExprObj {.importcpp: "nix::Expr", header: "nixexpr.hh".} = object
    discard
  Expr* = ptr ExprObj
  EvalState* {.importcpp: "std::shared_ptr<nix::EvalState>", header: "eval.hh".} = object
    discard

proc newEvalState*(store: Store): EvalState {.
  importcpp: "nix::newEvalState(@)", header: "seepuspus.hh", constructor.}

proc parseExprFromString*(state: EvalState; s, basePath: cstring): Expr {.
  importcpp: "#->parseExprFromString(@)".}

proc eval*(state: EvalState; expr: Expr; value: var ValueObj) {.
  importcpp: "#->eval(@)".}

proc forceValueDeep*(state: EvalState; value: var ValueObj) {.
  importcpp: "#->forceValueDeep(@)".}

proc stringView(state: EvalState; sym: Symbol): StringView {.
  importcpp: "((std::string_view)#->symbols[#])".}

proc symbolString*(state: EvalState; sym: Symbol): string = $stringView(state, sym)

proc initGC*() {.importcpp: "nix::initGC", header: "eval.hh".}
