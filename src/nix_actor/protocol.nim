
import
  preserves, std/tables

type
  Error* {.preservesRecord: "error".} = object
    `message`*: string

  Eval* {.preservesRecord: "eval".} = object
    `expr`*: string
    `log`* {.preservesEmbedded.}: Value
    `result`*: EvalResult

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

  Instantiate* {.preservesRecord: "instantiate".} = object
    `expr`*: string
    `log`* {.preservesEmbedded.}: Value
    `result`*: InstantiateResult

  Outputs* {.preservesRecord: "outputs".} = object
    `drv`*: string
    `storePaths`*: seq[string]

  ResolveDetail* {.preservesDictionary.} = object
    `command-path`*: seq[string]
    `lookupPath`*: seq[string]
    `options`*: AttrSet

proc `$`*(x: Error | Eval | AttrSet | Realise | Derivation | RealiseResult |
    EvalSuccess |
    EvalResult |
    InstantiateResult |
    ResolveStep |
    Instantiate |
    Outputs |
    ResolveDetail): string =
  `$`(toPreserves(x))

proc encode*(x: Error | Eval | AttrSet | Realise | Derivation | RealiseResult |
    EvalSuccess |
    EvalResult |
    InstantiateResult |
    ResolveStep |
    Instantiate |
    Outputs |
    ResolveDetail): seq[byte] =
  encode(toPreserves(x))
