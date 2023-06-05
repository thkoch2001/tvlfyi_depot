
import
  preserves, std/sets, std/tables

type
  Eval* {.preservesRecord: "eval".} = object
    `expr`*: string
    `options`*: Table[Symbol, Preserve[void]]
    `result`*: Preserve[void]

  Realise* {.preservesRecord: "realise".} = object
    `drv`*: string
    `outputs`*: seq[string]

  Missing* {.preservesRecord: "missing".} = object
    `targets`*: HashSet[string]
    `willBuild`*: HashSet[string]
    `willSubstitute`*: HashSet[string]
    `unknown`*: HashSet[string]
    `downloadSize`*: BiggestInt
    `narSize`*: BiggestInt

  Narinfo* {.preservesRecord: "narinfo".} = object
    `path`*: string
    `info`*: Dict

  FieldKind* {.pure.} = enum
    `int`, `string`
  `Field`* {.preservesOr.} = object
    case orKind*: FieldKind
    of FieldKind.`int`:
        `int`*: int

    of FieldKind.`string`:
        `string`*: string

  
  PathInfo* {.preservesRecord: "path-info".} = object
    `path`*: string
    `deriver`*: string
    `narHash`*: string
    `references`*: HashSet[string]
    `registrationTime`*: BiggestInt
    `narSize`*: BiggestInt
    `ultimate`*: bool
    `sigs`*: HashSet[string]
    `ca`*: string

  Dict* = Table[Symbol, Preserve[void]]
  Build* {.preservesRecord: "nix-build".} = object
    `input`*: string
    `output`*: Preserve[void]

  Fields* = seq[Field]
  ActionStart* {.preservesRecord: "start".} = object
    `id`*: BiggestInt
    `level`*: BiggestInt
    `type`*: BiggestInt
    `text`*: string
    `fields`*: Fields
    `parent`*: BiggestInt

  FieldString* = string
  Instantiate* {.preservesRecord: "instantiate".} = object
    `expr`*: string
    `options`*: Dict
    `result`*: Preserve[void]

  FieldInt* = BiggestInt
  ActionStop* {.preservesRecord: "stop".} = object
    `id`*: BiggestInt

  ActionResult* {.preservesRecord: "result".} = object
    `id`*: BiggestInt
    `type`*: BiggestInt
    `fields`*: Fields

proc `$`*(x: Eval | Realise | Missing | Narinfo | Field | PathInfo | Dict |
    Build |
    Fields |
    ActionStart |
    FieldString |
    Instantiate |
    FieldInt |
    ActionStop |
    ActionResult): string =
  `$`(toPreserve(x))

proc encode*(x: Eval | Realise | Missing | Narinfo | Field | PathInfo | Dict |
    Build |
    Fields |
    ActionStart |
    FieldString |
    Instantiate |
    FieldInt |
    ActionStop |
    ActionResult): seq[byte] =
  encode(toPreserve(x))
