We need a way to construct composable shells.

Shell environments should form a Monoid, so that we can combine them any way we like. Maybe even a lattice?

There’s a bunch of path modifications a shell can make:

* Add to a PATH-like (symbol lookup namespace)

  The complication here is that a PATH-like might have symbols that shadow other symbols.
  
  A PATH-like can be seen as a set of symbols that it exposes. The question is how these sets should be combined.
  
  If we see two symbols as the same if they have the same name (e.g. a binary that is called `echo` from GNU `coreutils` is the same as the binary `echo` from `busybox`, then they form a https://en.wikipedia.org/wiki/Partially_ordered_set.
  
  However, that is not a practical assumption in many cases, since the arguments vary considerably and using the wrong `echo` will lead to broken scripts at runtime (worst possible outcome).
  
  So we need a more restrictive policy.
  
  The easiest is forbidding joining PATH-likes that would shadow symbols. This can be mapped to a join semilattice where joining PATH-likes that shadow symbols leads to the shadowed symbols landing in a “shadowed” set.
  
  ```
  data PathLike =
    PathLike {
      nonShadowed :: Set Symbol,
      shadowed :: Set Symbol
    }
  ```
  
  ```
  join (PathLike { nonShadowed = [ "foo" "bar" ] })
       (PathLike { nonShadowed = [ "foo" "baz" ] })
    == (PathLike { shadowed = [ "foo" ]
                 , nonShadowed = [ "bar", "baz" ] })
  ```
  
  This will form a join-Semilattice again.
  
  In our model we need to get away from PATH-likes being just lists, because a linear shadowing mechanism gives us no real composability (it is ordering-dependent).
  
  However, we could have a more refined notion of equality of symbols, for example:
  
  * Two versions of `GNU hello` can be seen as equal, if their versions differ by a non-breaking change (according to semver)
  * … ?
  
* Set a variable
