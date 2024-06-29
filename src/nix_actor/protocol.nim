
import
  preserves, std/tables

type
  Error* {.preservesRecord: "error".} = object
    `message`*: string

  Eval* {.preservesRecord: "eval".} = object
    `expr`*: string
    `result`* {.preservesEmbedded.}: Value

  AttrSet* = Table[Symbol, Value]
  Realise* {.preservesRecord: "realise".} = object
    `drvPath`*: string
    `log`* {.preservesEmbedded.}: Value
    `outputs`* {.preservesEmbedded.}: Value

  Derivation* {.preservesRecord: "drv".} = object
    `drvPath`*: string
    `storePath`*: string

  RealiseResultKind* {.pure.} = enum
    `Error`, `RealiseSuccess`
  `RealiseResult`* {.preservesOr.} = object
    case orKind*: RealiseResultKind
    of RealiseResultKind.`Error`:
        `error`*: Error

    of RealiseResultKind.`RealiseSuccess`:
        `realisesuccess`*: RealiseSuccess

  
  EvalSuccess* {.preservesRecord: "ok".} = object
    `result`*: Value
    `expr`*: string

  RealiseSuccess* {.preservesRecord: "ok".} = object
    `storePath`*: string
    `drvPath`*: string

  EvalFile* {.preservesRecord: "eval-file".} = object
    `path`*: string
    `args`*: Value
    `result`* {.preservesEmbedded.}: Value

  EvalResultKind* {.pure.} = enum
    `err`, `ok`
  `EvalResult`* {.preservesOr.} = object
    case orKind*: EvalResultKind
    of EvalResultKind.`err`:
        `err`*: Error

    of EvalResultKind.`ok`:
        `ok`*: EvalSuccess

  
  ResolveStep* {.preservesRecord: "nix-actor".} = object
    `detail`*: ResolveDetail

  EvalFileResultKind* {.pure.} = enum
    `err`, `ok`
  `EvalFileResult`* {.preservesOr.} = object
    case orKind*: EvalFileResultKind
    of EvalFileResultKind.`err`:
        `err`*: Error

    of EvalFileResultKind.`ok`:
        `ok`*: EvalFileSuccess

  
  EvalFileSuccess* {.preservesRecord: "ok".} = object
    `result`*: Value
    `args`*: Value
    `path`*: string

  ResolveDetail* {.preservesDictionary.} = object
    `command-path`*: seq[string]
    `lookupPath`*: seq[string]
    `options`*: AttrSet

proc `$`*(x: Error | Eval | AttrSet | Realise | Derivation | RealiseResult |
    EvalSuccess |
    RealiseSuccess |
    EvalFile |
    EvalResult |
    ResolveStep |
    EvalFileResult |
    EvalFileSuccess |
    ResolveDetail): string =
  `$`(toPreserves(x))

proc encode*(x: Error | Eval | AttrSet | Realise | Derivation | RealiseResult |
    EvalSuccess |
    RealiseSuccess |
    EvalFile |
    EvalResult |
    ResolveStep |
    EvalFileResult |
    EvalFileSuccess |
    ResolveDetail): seq[byte] =
  encode(toPreserves(x))
