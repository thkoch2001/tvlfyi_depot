# SPDX-FileCopyrightText: ☭ Emery Hemingway
# SPDX-License-Identifier: Unlicense

type StdException* {.importcpp: "std::exception", header: "<exception>".} = object

proc what*(ex: StdException): cstring {.importcpp: "((char *)#.what())", nodecl.}


type StdString* {.importcpp: "std::string", header: "<string>".} = object

proc c_str*(s: StdString): cstring {.importcpp.}

type StringView* {.importcpp: "std::string_view", header: "<string>".} = object

proc toStringView*(s: pointer; count: int): StringView {.
  importcpp: "std::string_view(static_cast<const char *>(#), #)", constructor.}

proc toStringView*(s: string): StringView {.inline.} =
  if s.len == 0: toStringView(nil, 0)
  else: toStringView(unsafeAddr s[0], s.len)

proc toStringView*(buf: openarray[byte]): StringView {.inline.} =
  if buf.len == 0: toStringView(nil, 0)
  else: toStringView(unsafeAddr buf[0], buf.len)

proc toStringView*(sv: StringView): StringView {.inline.} = sv

proc data(sv: StringView): pointer {.importcpp.}
proc size(sv: StringView): csize_t {.importcpp.}

proc `$`*(sv: StringView): string =
  result = newString(sv.size)
  copyMem(addr result[0], sv.data, result.len)
