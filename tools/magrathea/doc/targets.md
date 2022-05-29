# Target syntax & semantics

## Syntax

<target> ::= <absolute-target> | <relative-target>

<absolute-target> ::= "//" <relative-target>

<relative-target> ::= <package>
                  | ":" <artifact>
                  | <package> ":" <artifact>

<package> ::= <component>
                  | <component> "/"
                  | <component> "/" <package>

<artifact> ::= <component>

<component> ::= "any string without '/' or ':'"


## Glossary

* Target: A full reference to an artifact, can either be relative (to the current file or working directory) or absolute (from the repository root).
* Package: A collection of artifacts and other packages
* Artifact: A build artifact (?A nix output?)

## Operational Semantics target -> nix

Semantically, a target is (Haskell data syntax):

```
data Target
  = Absolute RelativeTarget
  | Relative RelativeTarget

data RelativeTarget
  = RelativeTarget [Package] (Maybe Artifact)
```

First off, we need two paths, one for the current package and one for the root of the monorepo



```
-------------------------------------------(repo-root)
RepoRoot => <nix path to the root of the repository>

-----------------------(current-package)
CurrentPackage => ./default.nix
```

We need a way of indexing into a nix object

```
e => v ∈ Nix Attrset
-----------(index-into)
IndexInto(key, e) => v.${key}
```

If the artifact is empty, we default to the "default" artifact

```
RelativeTarget packages Nothing => v
---------------------------------------(default-target)
RelativeTarget packages (Just "default") => v
```


Given a relative Target, we can TODO



Example:

Absolute (RelativeTarget ["tools", "magrathea"] Nothing) => v
=> (default-target)
Absolute (RelativeTarget ["tools", "magrathea"] (Just "default")) => v
=> … TODO
