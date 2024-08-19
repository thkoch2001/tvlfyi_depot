
import
  preserves, std/tables, std/options

type
  Error* {.preservesRecord: "error".} = object
    `message`*: string

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
    `import`*: string
    `lookupPath`*: seq[string]
    `store`*: Option[Value]

  Derivation* {.preservesRecord: "drv".} = object
    `drvPath`*: string
    `storePath`*: string

  StoreResolveDetail* {.preservesDictionary.} = object
    `params`*: AttrSet
    `uri`*: string

  CheckStorePath* {.preservesRecord: "check-path".} = object
    `path`*: string
    `valid`* {.preservesEmbedded.}: EmbeddedRef

  StoreResolveStep* {.preservesRecord: "nix-store".} = object
    `detail`*: StoreResolveDetail

proc `$`*(x: Error | RepoArgs | RepoResolveStep | AttrSet | RepoStore |
    RepoResolveDetail |
    Derivation |
    StoreResolveDetail |
    CheckStorePath |
    StoreResolveStep): string =
  `$`(toPreserves(x))

proc encode*(x: Error | RepoArgs | RepoResolveStep | AttrSet | RepoStore |
    RepoResolveDetail |
    Derivation |
    StoreResolveDetail |
    CheckStorePath |
    StoreResolveStep): seq[byte] =
  encode(toPreserves(x))
