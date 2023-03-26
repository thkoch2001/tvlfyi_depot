
import
  std/typetraits, preserves

type
  Serve* {.preservesRecord: "serve".} = object
    `cap`* {.preservesEmbedded.}: Preserve[void]

  Build* {.preservesRecord: "nix-build".} = object
    `input`*: string
    `output`*: Preserve[void]

proc `$`*(x: Serve | Build): string =
  `$`(toPreserve(x))

proc encode*(x: Serve | Build): seq[byte] =
  encode(toPreserve(x))
