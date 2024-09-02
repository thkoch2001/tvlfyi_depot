
import
  preserves, std/tables, std/options

type
  EvalResolveDetailLookupPath* = Option[seq[string]]
  EvalResolveDetailStoreUri* = Option[string]
  `EvalResolveDetail`* {.preservesDictionary.} = object
    `lookupPath`*: Option[seq[string]]
    `storeParams`*: Option[AttrSet]
    `storeUri`*: Option[string]

  Eval* {.preservesRecord: "eval".} = object
    `expr`*: string
    `args`*: Value
    `result`* {.preservesEmbedded.}: EmbeddedRef

  Error* {.preservesRecord: "error".} = object
    `message`*: Value

  AttrSet* = Table[Symbol, Value]
  LookupPathKind* {.pure.} = enum
    `lookupPath`, `absent`
  LookupPathLookupPath* {.preservesDictionary.} = object
    `lookupPath`*: seq[string]

  LookupPathAbsent* {.preservesDictionary.} = object
  
  `LookupPath`* {.preservesOr.} = object
    case orKind*: LookupPathKind
    of LookupPathKind.`lookupPath`:
        `lookuppath`*: LookupPathLookupPath

    of LookupPathKind.`absent`:
        `absent`*: LookupPathAbsent

  
  StoreResolveDetailStoreUri* = string
  `StoreResolveDetail`* {.preservesDictionary.} = object
    `storeParams`*: Option[AttrSet]
    `storeUri`*: string

  StoreParamsKind* {.pure.} = enum
    `storeParams`, `absent`
  StoreParamsStoreParams* {.preservesDictionary.} = object
    `storeParams`*: AttrSet

  StoreParamsAbsent* {.preservesDictionary.} = object
  
  `StoreParams`* {.preservesOr.} = object
    case orKind*: StoreParamsKind
    of StoreParamsKind.`storeParams`:
        `storeparams`*: StoreParamsStoreParams

    of StoreParamsKind.`absent`:
        `absent`*: StoreParamsAbsent

  
  ResultKind* {.pure.} = enum
    `Error`, `ok`
  ResultOk* {.preservesRecord: "ok".} = object
    `value`*: Value

  `Result`* {.preservesOr.} = object
    case orKind*: ResultKind
    of ResultKind.`Error`:
        `error`*: Error

    of ResultKind.`ok`:
        `ok`*: ResultOk

  
  RealiseString* {.preservesRecord: "realise-string".} = object
    `result`* {.preservesEmbedded.}: EmbeddedRef

  CheckStorePath* {.preservesRecord: "check-path".} = object
    `path`*: string
    `valid`* {.preservesEmbedded.}: EmbeddedRef

  StoreUriKind* {.pure.} = enum
    `storeUri`, `absent`
  StoreUriStoreUri* {.preservesDictionary.} = object
    `storeUri`*: string

  StoreUriAbsent* {.preservesDictionary.} = object
  
  `StoreUri`* {.preservesOr.} = object
    case orKind*: StoreUriKind
    of StoreUriKind.`storeUri`:
        `storeuri`*: StoreUriStoreUri

    of StoreUriKind.`absent`:
        `absent`*: StoreUriAbsent

  
  Replicate* {.preservesRecord: "replicate".} = object
    `target`* {.preservesEmbedded.}: EmbeddedRef
    `storePath`*: string
    `result`* {.preservesEmbedded.}: EmbeddedRef

  StoreResolveStep* {.preservesRecord: "nix-store".} = object
    `detail`*: StoreResolveDetail

  EvalResolveStep* {.preservesRecord: "nix".} = object
    `detail`*: EvalResolveDetail

proc `$`*(x: EvalResolveDetail | Eval | Error | AttrSet | LookupPath |
    StoreResolveDetail |
    StoreParams |
    Result |
    RealiseString |
    CheckStorePath |
    StoreUri |
    Replicate |
    StoreResolveStep |
    EvalResolveStep): string =
  `$`(toPreserves(x))

proc encode*(x: EvalResolveDetail | Eval | Error | AttrSet | LookupPath |
    StoreResolveDetail |
    StoreParams |
    Result |
    RealiseString |
    CheckStorePath |
    StoreUri |
    Replicate |
    StoreResolveStep |
    EvalResolveStep): seq[byte] =
  encode(toPreserves(x))
