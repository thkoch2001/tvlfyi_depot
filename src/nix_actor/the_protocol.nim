
import
  std/typetraits, preserves

type
  Build* {.preservesRecord: "nix-build".} = object
    `input`*: string
    `output`*: string

proc `$`*(x: Build): string =
  `$`(toPreserve(x))

proc encode*(x: Build): seq[byte] =
  encode(toPreserve(x))
