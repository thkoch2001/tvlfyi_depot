# SPDX-FileCopyrightText: ☭ Emery Hemingway
# SPDX-License-Identifier: Unlicense

{.passC: staticExec"$PKG_CONFIG --cflags nix-store".}
{.passL: staticExec"$PKG_CONFIG --libs nix-store".}

{.passC: "'-DSYSTEM=\"x86_64-linux\"'".}

type StdString {.importcpp: "std::string", header: "<string>".} = object
proc data(s: StdString): pointer {.importcpp: "#.data()".}
proc len(s: StdString): csize_t {.importcpp: "#.length()".}
proc `$`*(cpp: StdString): string =
  result.setLen(cpp.len)
  if result.len > 0:
    copyMem(addr result[0], cpp.data, result.len)

type
  StorePath {.importcpp: "nix::StorePath", header: "path.hh".} = object
    discard

var nixVersion* {.importc: "nix::nixVersion", header: "globals.hh".}: StdString

proc isDerivation*(path: StorePath): bool {.importcpp.}

type
  Store* {.importcpp: "nix::ref<nix::Store>", header: "store-api.hh".} = object
    discard

proc ensurePath*(store: Store; path: StorePath) {.importcpp.}

proc openStore*(): Store {.importcpp: "nix::openStore".}
