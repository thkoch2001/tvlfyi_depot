
import
  preserves, std/tables, std/options

type
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

  
  NixResolveStep* {.preservesRecord: "nix".} = object
    `detail`*: NixResolveDetail

  RealiseString* {.preservesRecord: "realise-string".} = object
    `result`* {.preservesEmbedded.}: EmbeddedRef

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

  
  NixResolveDetailLookupPath* = Option[seq[string]]
  NixResolveDetailStoreUri* = Option[string]
  `NixResolveDetail`* {.preservesDictionary.} = object
    `lookupPath`*: Option[seq[string]]
    `storeParams`*: Option[AttrSet]
    `storeUri`*: Option[string]

proc `$`*(x: Eval | Error | AttrSet | LookupPath | Result | StoreParams |
    NixResolveStep |
    RealiseString |
    StoreUri |
    NixResolveDetail): string =
  `$`(toPreserves(x))

proc encode*(x: Eval | Error | AttrSet | LookupPath | Result | StoreParams |
    NixResolveStep |
    RealiseString |
    StoreUri |
    NixResolveDetail): seq[byte] =
  encode(toPreserves(x))
