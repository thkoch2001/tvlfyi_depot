## Module generated by c2nim for nix_api_value.h

import ./nix_api_types

{.pragma: nix_api_value, header: "nix_api_value.h", importc: "nix_$1".}

type
  BindingsBuilder* {.header: "nix_api_value.h", importc.} = distinct pointer
  ExternalValue* {.header: "nix_api_value.h", importc.} = distinct pointer
  ListBuilder* {.header: "nix_api_value.h", importc.} = distinct pointer
  RealisedString* {.header: "nix_api_value.h", importc: "nix_realised_string".} = distinct pointer

proc isNil*(p: BindingsBuilder|ExternalValue|ListBuilder|RealisedString): bool =
  cast[pointer](p).isNil

proc alloc_value*(context: NixContext; state: EvalState): Value {.nix_api_value.}

proc get_type*(context: NixContext; value: Value): ValueType {.nix_api_value.}

proc get_typename*(context: NixContext; value: Value): cstring {.nix_api_value.}

proc get_bool*(context: NixContext; value: Value): bool {.nix_api_value.}

proc get_string*(context: NixContext; value: Value; callback: GetStringCallback; user_data: pointer): nix_err {.nix_api_value.}

proc get_path_string*(context: NixContext; value: Value): cstring {.nix_api_value.}

proc get_list_size*(context: NixContext; value: Value): cuint {.nix_api_value.}

proc get_attrs_size*(context: NixContext; value: Value): cuint {.nix_api_value.}

proc get_float*(context: NixContext; value: Value): cdouble {.nix_api_value.}

proc get_int*(context: NixContext; value: Value): int64 {.nix_api_value.}

proc get_external*(context: NixContext; a2: Value): ExternalValue {.nix_api_value.}

proc get_list_byidx*(context: NixContext; value: Value; state: EvalState; ix: cuint): Value {.nix_api_value.}

proc get_attr_byname*(context: NixContext; value: Value; state: EvalState; name: cstring): Value {.nix_api_value.}

proc has_attr_byname*(context: NixContext; value: Value; state: EvalState; name: cstring): bool {.nix_api_value.}

proc get_attr_byidx*(context: NixContext; value: Value; state: EvalState; i: cuint; name: ptr cstring): Value {.nix_api_value.}

# proc get_attr_name_byidx*(context: NixContext; value: Value; state: EvalState; i: cuint): cstring {.nix_api_value.}

proc init_bool*(context: NixContext; value: Value; b: bool): nix_err {.nix_api_value.}

proc init_string*(context: NixContext; value: Value; str: cstring): nix_err {.nix_api_value.}

proc init_path_string*(context: NixContext; s: EvalState; value: Value; str: cstring): nix_err {.nix_api_value.}

proc init_float*(context: NixContext; value: Value; d: cdouble): nix_err {.nix_api_value.}

proc init_int*(context: NixContext; value: Value; i: int64): nix_err {.nix_api_value.}

proc init_null*(context: NixContext; value: Value): nix_err {.nix_api_value.}

proc init_apply*(context: NixContext; value: Value; fn: Value; arg: Value): nix_err {.nix_api_value.}

proc init_external*(context: NixContext; value: Value; val: ExternalValue): nix_err {.nix_api_value.}

proc make_list*(context: NixContext; list_builder: ListBuilder; value: Value): nix_err {.nix_api_value.}

proc make_list_builder*(context: NixContext; state: EvalState; capacity: csize_t): ListBuilder {.nix_api_value.}

proc list_builder_insert*(context: NixContext; list_builder: ListBuilder; index: cuint; value: Value): nix_err {.nix_api_value.}

proc list_builder_free*(list_builder: ListBuilder) {.nix_api_value.}

proc make_attrs*(context: NixContext; value: Value; b: BindingsBuilder): nix_err {.nix_api_value.}

# proc init_primop*(context: NixContext; value: Value; op: PrimOp): nix_err {.nix_api_value.}

proc copy_value*(context: NixContext; value: Value; source: Value): nix_err {.nix_api_value.}

proc make_bindings_builder*(context: NixContext; state: EvalState; capacity: csize_t): BindingsBuilder {.nix_api_value.}

proc bindings_builder_insert*(context: NixContext; builder: BindingsBuilder; name: cstring; value: Value): nix_err {.nix_api_value.}

proc bindings_builder_free*(builder: BindingsBuilder) {.nix_api_value.}

proc string_realise*(context: NixContext; state: EvalState; value: Value; isIFD: bool): RealisedString {.nix_api_value.}

proc realised_string_get_buffer_start*(realised_string: RealisedString): cstring {.nix_api_value.}

proc realised_string_get_buffer_size*(realised_string: RealisedString): csize_t {.nix_api_value.}

proc realised_string_get_store_path_count*(realised_string: RealisedString): csize_t {.nix_api_value.}

proc realised_string_get_store_path*(realised_string: RealisedString; index: csize_t): StorePath {.nix_api_value.}

proc realised_string_free*(realised_string: RealisedString) {.nix_api_value.}
