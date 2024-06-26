
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
    `drv`*: string
    `log`* {.preservesEmbedded.}: Value
    `outputs`* {.preservesEmbedded.}: Value

  Derivation* {.preservesRecord: "drv".} = object
    `expr`*: string
    `storePath`*: string

  RealiseResultKind* {.pure.} = enum
    `Error`, `Outputs`
  `RealiseResult`* {.preservesOr.} = object
    case orKind*: RealiseResultKind
    of RealiseResultKind.`Error`:
        `error`*: Error

    of RealiseResultKind.`Outputs`:
        `outputs`*: Outputs

  
  EvalSuccess* {.preservesTuple.} = object
    `expr`*: string
    `result`*: Value

  EvalFile* {.preservesRecord: "eval-file".} = object
    `path`*: string
    `args`*: Value
    `result`* {.preservesEmbedded.}: Value

  EvalResultKind* {.pure.} = enum
    `Error`, `EvalSuccess`
  `EvalResult`* {.preservesOr.} = object
    case orKind*: EvalResultKind
    of EvalResultKind.`Error`:
        `error`*: Error

    of EvalResultKind.`EvalSuccess`:
        `evalsuccess`*: EvalSuccess

  
  InstantiateResultKind* {.pure.} = enum
    `Error`, `Derivation`
  `InstantiateResult`* {.preservesOr.} = object
    case orKind*: InstantiateResultKind
    of InstantiateResultKind.`Error`:
        `error`*: Error

    of InstantiateResultKind.`Derivation`:
        `derivation`*: Derivation

  
  ResolveStep* {.preservesRecord: "nix-actor".} = object
    `detail`*: ResolveDetail

  EvalFileResultKind* {.pure.} = enum
    `Error`, `success`
  `EvalFileResult`* {.preservesOr.} = object
    case orKind*: EvalFileResultKind
    of EvalFileResultKind.`Error`:
        `error`*: Error

    of EvalFileResultKind.`success`:
        `success`*: EvalFileSuccess

  
  Instantiate* {.preservesRecord: "instantiate".} = object
    `expr`*: string
    `log`* {.preservesEmbedded.}: Value
    `result`*: InstantiateResult

  EvalFileSuccess* {.preservesTuple.} = object
    `path`*: string
    `args`*: Value
    `result`*: Value

  Outputs* {.preservesRecord: "outputs".} = object
    `drv`*: string
    `storePaths`*: seq[string]

  ResolveDetail* {.preservesDictionary.} = object
    `command-path`*: seq[string]
    `lookupPath`*: seq[string]
    `options`*: AttrSet

proc `$`*(x: Error | Eval | AttrSet | Realise | Derivation | RealiseResult |
    EvalSuccess |
    EvalFile |
    EvalResult |
    InstantiateResult |
    ResolveStep |
    EvalFileResult |
    Instantiate |
    EvalFileSuccess |
    Outputs |
    ResolveDetail): string =
  `$`(toPreserves(x))

proc encode*(x: Error | Eval | AttrSet | Realise | Derivation | RealiseResult |
    EvalSuccess |
    EvalFile |
    EvalResult |
    InstantiateResult |
    ResolveStep |
    EvalFileResult |
    Instantiate |
    EvalFileSuccess |
    Outputs |
    ResolveDetail): seq[byte] =
  encode(toPreserves(x))
