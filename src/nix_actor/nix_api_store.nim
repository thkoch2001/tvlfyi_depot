## Module generated by c2nim for nix_api_store.h

import ./nix_api_types

{.passC: staticExec"$PKG_CONFIG --cflags nix-store-c".}
{.passL: staticExec"$PKG_CONFIG --libs nix-store-c".}

{.pragma: nix_api_store, header: "nix_api_store.h", importc: "nix_$1".}

proc libstore_init*(context: NixContext): nix_err {.nix_api_store.}

proc libstore_init_no_load_config*(context: NixContext): nix_err {.nix_api_store.}

proc init_plugins*(context: NixContext): nix_err {.nix_api_store.}

proc store_open*(context: NixContext; uri: cstring; params: ptr cstringArray): Store {.nix_api_store.}

proc store_free*(store: Store) {.nix_api_store.}

proc store_get_uri*(context: NixContext; store: Store; callback: GetStringCallback; user_data: pointer): nix_err {.nix_api_store.}

proc store_parse_path*(context: NixContext; store: Store; path: cstring): StorePath {.nix_api_store.}

proc store_path_name*(store_path: StorePath; callback: GetStringCallback; user_data: pointer) {.nix_api_store.}

proc store_path_clone*(p: StorePath): StorePath {.nix_api_store.}

proc store_path_free*(p: StorePath) {.nix_api_store.}

proc store_is_valid_path*(context: NixContext; store: Store; path: StorePath): bool {.nix_api_store.}

type RealiseCallback* = proc (userdata: pointer; outname: cstring; `out`: cstring) {.cdecl.}

proc store_realise*(context: NixContext; store: Store; path: StorePath; userdata: pointer; callback: RealiseCallback): nix_err {.nix_api_store.}

proc store_get_version*(context: NixContext; store: Store; callback: GetStringCallback; user_data: pointer): nix_err {.nix_api_store.}

proc store_copy_closure*(context: NixContext; src, dst: Store; path: StorePath): nix_err {.nix_api_store.}
