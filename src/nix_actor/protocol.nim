
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

  
  Derivation* {.preservesRecord: "drv".} = object
    `value`*: Value
    `context`*: Value

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

  
  NixResolveDetailCache* = Option[EmbeddedRef]
  NixResolveDetailLookupPath* = Option[seq[string]]
  NixResolveDetailStoreUri* = Option[string]
  `NixResolveDetail`* {.preservesDictionary.} = object
    `cache`*: Option[EmbeddedRef]
    `lookupPath`*: Option[seq[string]]
    `storeParams`*: Option[AttrSet]
    `storeUri`*: Option[string]

  CopyClosure* {.preservesRecord: "copy-closure".} = object
    `dest`* {.preservesEmbedded.}: EmbeddedRef
    `storePath`*: string
    `result`* {.preservesEmbedded.}: EmbeddedRef

  CacheSpaceKind* {.pure.} = enum
    `cacheSpace`, `absent`
  CacheSpaceCacheSpace* {.preservesDictionary.} = object
    `cache`* {.preservesEmbedded.}: EmbeddedRef

  CacheSpaceAbsent* {.preservesDictionary.} = object
  
  `CacheSpace`* {.preservesOr.} = object
    case orKind*: CacheSpaceKind
    of CacheSpaceKind.`cacheSpace`:
        `cachespace`* {.preservesEmbedded.}: CacheSpaceCacheSpace

    of CacheSpaceKind.`absent`:
        `absent`*: CacheSpaceAbsent

  
proc `$`*(x: Eval | Error | AttrSet | LookupPath | Derivation | Result |
    StoreParams |
    NixResolveStep |
    RealiseString |
    CheckStorePath |
    StoreUri |
    NixResolveDetail |
    CopyClosure |
    CacheSpace): string =
  `$`(toPreserves(x))

proc encode*(x: Eval | Error | AttrSet | LookupPath | Derivation | Result |
    StoreParams |
    NixResolveStep |
    RealiseString |
    CheckStorePath |
    StoreUri |
    NixResolveDetail |
    CopyClosure |
    CacheSpace): seq[byte] =
  encode(toPreserves(x))
