
import
  preserves, std/tables, std/options

type
  Error* {.preservesRecord: "error".} = object
    `message`*: Value

  RepoArgsKind* {.pure.} = enum
    `present`, `absent`
  RepoArgsPresent* {.preservesDictionary.} = object
    `args`*: Value

  RepoArgsAbsent* {.preservesDictionary.} = object
  
  `RepoArgs`* {.preservesOr.} = object
    case orKind*: RepoArgsKind
    of RepoArgsKind.`present`:
        `present`*: RepoArgsPresent

    of RepoArgsKind.`absent`:
        `absent`*: RepoArgsAbsent

  
  RepoResolveStep* {.preservesRecord: "nix-repo".} = object
    `detail`*: RepoResolveDetail

  AttrSet* = Table[Symbol, Value]
  RepoStoreKind* {.pure.} = enum
    `uri`, `cap`, `absent`
  RepoStoreUri* {.preservesDictionary.} = object
    `store`*: string

  RepoStoreCap* {.preservesDictionary.} = object
    `store`* {.preservesEmbedded.}: EmbeddedRef

  RepoStoreAbsent* {.preservesDictionary.} = object
  
  `RepoStore`* {.preservesOr.} = object
    case orKind*: RepoStoreKind
    of RepoStoreKind.`uri`:
        `uri`*: RepoStoreUri

    of RepoStoreKind.`cap`:
        `cap`* {.preservesEmbedded.}: RepoStoreCap

    of RepoStoreKind.`absent`:
        `absent`*: RepoStoreAbsent

  
  RepoResolveDetailArgs* = Option[Value]
  RepoResolveDetailImport* = string
  RepoResolveDetailLookupPath* = seq[string]
  RepoResolveDetailStore* = Option[Value]
  `RepoResolveDetail`* {.preservesDictionary.} = object
    `args`*: Option[Value]
    `cache`*: Option[EmbeddedRef]
    `import`*: string
    `lookupPath`*: seq[string]
    `store`*: Option[Value]

  Derivation* {.preservesRecord: "drv".} = object
    `drvPath`*: string
    `storePath`*: string

  StoreResolveDetailCache* = Option[EmbeddedRef]
  StoreResolveDetailUri* = string
  `StoreResolveDetail`* {.preservesDictionary.} = object
    `cache`*: Option[EmbeddedRef]
    `params`*: AttrSet
    `uri`*: string

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

  
  CheckStorePath* {.preservesRecord: "check-path".} = object
    `path`*: string
    `valid`* {.preservesEmbedded.}: EmbeddedRef

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

  
  StoreResolveStep* {.preservesRecord: "nix-store".} = object
    `detail`*: StoreResolveDetail

proc `$`*(x: Error | RepoArgs | RepoResolveStep | AttrSet | RepoStore |
    RepoResolveDetail |
    Derivation |
    StoreResolveDetail |
    Result |
    CheckStorePath |
    CopyClosure |
    CacheSpace |
    StoreResolveStep): string =
  `$`(toPreserves(x))

proc encode*(x: Error | RepoArgs | RepoResolveStep | AttrSet | RepoStore |
    RepoResolveDetail |
    Derivation |
    StoreResolveDetail |
    Result |
    CheckStorePath |
    CopyClosure |
    CacheSpace |
    StoreResolveStep): seq[byte] =
  encode(toPreserves(x))
