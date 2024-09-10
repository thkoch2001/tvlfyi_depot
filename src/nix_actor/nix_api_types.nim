type
  nix_err* = cint
  EvalState* {.header: "nix_api_expr.h", importc.} = distinct pointer
  NixContext* {.header: "nix_api_util.h", importc: "nix_c_context".} = distinct pointer
  NixException* = object of CatchableError
  Store* {.header: "nix_api_store.h", importc.} = distinct pointer
  StorePath* {.header: "nix_api_store.h", importc.} = distinct pointer
  Value* {.header: "nix_api_value.h", importc.} = distinct pointer
  ValueType* {.header: "nix_api_value.h", importc.} = enum
    NIX_TYPE_THUNK,
    NIX_TYPE_INT,
    NIX_TYPE_FLOAT,
    NIX_TYPE_BOOL,
    NIX_TYPE_STRING,
    NIX_TYPE_PATH,
    NIX_TYPE_NULL,
    NIX_TYPE_ATTRS,
    NIX_TYPE_LIST,
    NIX_TYPE_FUNCTION,
    NIX_TYPE_EXTERNAL

  GetStringCallback* = proc (start: cstring; n: cuint; data: pointer) {.cdecl.}

proc isNil*(p: EvalState): bool {.borrow.}
proc isNil*(p: NixContext): bool {.borrow.}
proc isNil*(p: Store): bool {.borrow.}
proc isNil*(p: StorePath): bool {.borrow.}
proc isNil*(p: Value): bool {.borrow.}
