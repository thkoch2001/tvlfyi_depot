
import
  preserves, std/sets, std/tables

type
  Eval* {.preservesRecord: "eval".} = object
    `expr`*: string
    `options`*: Table[Symbol, Preserve[void]]
    `result`*: Preserve[void]

  AttrSet* = Table[Symbol, Preserve[void]]
  Realise* {.preservesRecord: "realise".} = object
    `drv`*: string
    `outputs`*: StringSeq

  LegacyPathAttrs* {.preservesDictionary.} = object
    `ca`*: string
    `deriver`*: string
    `narHash`*: string
    `narSize`*: BiggestInt
    `references`*: StringSeq
    `registrationTime`*: BiggestInt
    `sigs`*: StringSet
    `ultimate`*: bool

  Missing* {.preservesRecord: "missing".} = object
    `targets`*: StringSeq
    `willBuild`*: StringSet
    `willSubstitute`*: StringSet
    `unknown`*: StringSet
    `downloadSize`*: BiggestInt
    `narSize`*: BiggestInt

  Narinfo* {.preservesRecord: "narinfo".} = object
    `path`*: string
    `info`*: AttrSet

  FieldKind* {.pure.} = enum
    `int`, `string`
  `Field`* {.preservesOr.} = object
    case orKind*: FieldKind
    of FieldKind.`int`:
        `int`*: int

    of FieldKind.`string`:
        `string`*: string

  
  StringSet* = HashSet[string]
  AddToStoreAttrs* {.preservesDictionary.} = object
    `ca`*: string
    `ca-method`*: Symbol
    `deriver`*: string
    `eris`*: seq[byte]
    `name`*: string
    `narHash`*: string
    `narSize`*: BiggestInt
    `references`*: StringSeq
    `registrationTime`*: BiggestInt
    `sigs`*: StringSet
    `ultimate`*: bool

  AddToStoreClientAttrs* {.preservesDictionary.} = object
    `ca-method`*: Symbol
    `eris`*: seq[byte]
    `name`*: string
    `references`*: StringSeq

  PathInfo* {.preservesRecord: "path".} = object
    `path`*: string
    `attrs`*: AttrSet

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

  Instantiate* {.preservesRecord: "instantiate".} = object
    `expr`*: string
    `options`*: AttrSet
    `result`*: Preserve[void]

  StringSeq* = seq[string]
  ActionStop* {.preservesRecord: "stop".} = object
    `id`*: BiggestInt

  ActionResult* {.preservesRecord: "result".} = object
    `id`*: BiggestInt
    `type`*: BiggestInt
    `fields`*: Fields

proc `$`*(x: Eval | AttrSet | Realise | LegacyPathAttrs | Missing | Narinfo |
    Field |
    StringSet |
    AddToStoreAttrs |
    AddToStoreClientAttrs |
    PathInfo |
    Build |
    Fields |
    ActionStart |
    Instantiate |
    StringSeq |
    ActionStop |
    ActionResult): string =
  `$`(toPreserve(x))

proc encode*(x: Eval | AttrSet | Realise | LegacyPathAttrs | Missing | Narinfo |
    Field |
    StringSet |
    AddToStoreAttrs |
    AddToStoreClientAttrs |
    PathInfo |
    Build |
    Fields |
    ActionStart |
    Instantiate |
    StringSeq |
    ActionStop |
    ActionResult): seq[byte] =
  encode(toPreserve(x))
