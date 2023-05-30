
import
  preserves, std/tables

type
  Eval* {.preservesRecord: "eval".} = object
    `expr`*: string
    `options`*: Table[Symbol, Preserve[void]]
    `result`*: Preserve[void]

  Realise* {.preservesRecord: "realise".} = object
    `drv`*: string
    `outputs`*: seq[string]

  Build* {.preservesRecord: "nix-build".} = object
    `input`*: string
    `output`*: Preserve[void]

proc `$`*(x: Eval | Realise | Build): string =
  `$`(toPreserve(x))

proc encode*(x: Eval | Realise | Build): seq[byte] =
  encode(toPreserve(x))
