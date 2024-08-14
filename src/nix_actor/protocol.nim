
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
  RepoResolveDetailArgs* = Option[Value]
  RepoResolveDetailImport* = string
  RepoResolveDetailLookupPath* = seq[string]
  RepoResolveDetailStore* = string
  `RepoResolveDetail`* {.preservesDictionary.} = object
    `args`*: Option[Value]
    `import`*: string
    `lookupPath`*: seq[string]
    `store`*: string

  Derivation* {.preservesRecord: "drv".} = object
    `drvPath`*: string
    `storePath`*: string

  StoreResolveDetail* {.preservesDictionary.} = object
    `params`*: AttrSet
    `uri`*: string

  CheckStorePath* {.preservesRecord: "check-path".} = object
    `path`*: string
    `valid`* {.preservesEmbedded.}: Value

  StoreResolveStep* {.preservesRecord: "nix-store".} = object
    `detail`*: StoreResolveDetail

proc `$`*(x: Error | RepoArgs | RepoResolveStep | AttrSet | RepoResolveDetail |
    Derivation |
    StoreResolveDetail |
    CheckStorePath |
    StoreResolveStep): string =
  `$`(toPreserves(x))

proc encode*(x: Error | RepoArgs | RepoResolveStep | AttrSet | RepoResolveDetail |
    Derivation |
    StoreResolveDetail |
    CheckStorePath |
    StoreResolveStep): seq[byte] =
  encode(toPreserves(x))
