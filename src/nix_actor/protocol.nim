
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

  Narinfo* {.preservesRecord: "narinfo".} = object
    `path`*: string
    `info`*: Dict

  Dict* = Table[Symbol, Preserve[void]]
  Build* {.preservesRecord: "nix-build".} = object
    `input`*: string
    `output`*: Preserve[void]

  Instantiate* {.preservesRecord: "instantiate".} = object
    `expr`*: string
    `options`*: Dict
    `result`*: Preserve[void]

proc `$`*(x: Eval | Realise | Narinfo | Dict | Build | Instantiate): string =
  `$`(toPreserve(x))

proc encode*(x: Eval | Realise | Narinfo | Dict | Build | Instantiate): seq[byte] =
  encode(toPreserve(x))
