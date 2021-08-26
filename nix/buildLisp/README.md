buildLisp.nix
=============

This is a build system for Common Lisp, written in Nix.

It aims to offer an alternative to ASDF for users who live in a
Nix-based ecosystem. This offers several advantages over ASDF:

* Simpler (almost logic-less) package definitions
* Easy linking of native dependencies (from Nix)
* Composability with Nix tooling for other languages
* Effective, per-system caching strategies
* Easy overriding of dependencies and whatnot
* Convenient support for multiple Common Lisp implementations
* ... and more!

The project is still in its early stages and some important
restrictions should be highlighted:

* Extending `buildLisp` with support for a custom implementation
  currently requires some knowledge of internals and may not be
  considered stable yet.
* Parallel compilation is not possible: Since buildLisp doesn't encode
  dependencies between components (i. e. source files) like ASDF,
  it must compile source files in sequence to avoid errors due to
  undefined symbols.

## Usage

`buildLisp` exposes four different functions:

* `buildLisp.library`: Builds a collection of Lisp files into a library.

  | parameter | type         | use                           | required? |
  |-----------|--------------|-------------------------------|-----------|
  | `name`    | `string`     | Name of the library           | yes       |
  | `srcs`    | `list<path>` | List of paths to source files | yes       |
  | `deps`    | `list<drv>`  | List of dependencies          | no        |
  | `native`  | `list<drv>`  | List of native dependencies   | no        |
  | `test`    | see "Tests"  | Specification for test suite  | no        |
  | `implementation` | see "Implementations" | Common Lisp implementation to use | no |

  The output of invoking this is a directory containing a FASL file
  that is the concatenated result of all compiled sources.

* `buildLisp.program`: Builds an executable program out of Lisp files.

  | parameter | type         | use                           | required? |
  |-----------|--------------|-------------------------------|-----------|
  | `name`    | `string`     | Name of the program           | yes       |
  | `srcs`    | `list<path>` | List of paths to source files | yes       |
  | `deps`    | `list<drv>`  | List of dependencies          | no        |
  | `native`  | `list<drv>`  | List of native dependencies   | no        |
  | `main`    | `string`     | Entrypoint function           | no        |
  | `test`    | see "Tests"  | Specification for test suite  | no        |
  | `implementation` | see "Implementations" | Common Lisp implementation to use | no |

  The `main` parameter should be the name of a function and defaults
  to `${name}:main` (i.e. the *exported* `main` function of the
  package named after the program).

  The output of invoking this is a directory containing a
  `bin/${name}`.

* `buildLisp.bundled`: Creates a virtual dependency on a built-in library.

  Certain libraries ship with Lisp implementations, for example
  UIOP/ASDF are commonly included but many implementations also ship
  internals (such as SBCLs various `sb-*` libraries).

  This function takes a single string argument that is the name of a
  built-in library and returns a "package" that simply requires this
  library.

## Tests

Both `buildLisp.library` and `buildLisp.program` take an optional argument
`tests`, which has the following supported fields:

  | parameter    | type         | use                           | required? |
  |--------------|--------------|-------------------------------|-----------|
  | `name`       | `string`     | Name of the test suite        | no        |
  | `expression` | `string`     | Lisp expression to run tests  | yes       |
  | `srcs`       | `list<path>` | List of paths to source files | no        |
  | `native`     | `list<drv>`  | List of native dependencies   | no        |

the `expression` parameter should be a Lisp expression and will be evaluated
after loading all sources and dependencies (including library/program
dependencies). It must return a non-`NIL` value if the test suite has passed.

## Example

Using buildLisp could look like this:

```nix
{ buildLisp, lispPkgs }:

let libExample = buildLisp.library {
    name = "lib-example";
    srcs = [ ./lib.lisp ];

    deps = with lispPkgs; [
      (buildLisp.bundled "sb-posix")
      iterate
      cl-ppcre
    ];
};
in buildLisp.program {
    name = "example";
    deps = [ libExample ];
    srcs = [ ./main.lisp ];
    tests = {
      deps = [ lispPkgs.fiveam ];
      srcs = [ ./tests.lisp ];
      expression = "(fiveam:run!)";
    };
}
```

## Development REPLs

`buildLisp` builds loadable variants of both `program` and `library` derivations
(usually FASL files). Therefore it can provide a convenient way to obtain an
instance of any implementation preloaded with `buildLisp`-derivations. This
is especially useful to use as a host for Sly or SLIME.

* `buildLisp.sbcl.lispWith`, `buildLisp.ccl.lispWith`, ...:
  Creates a wrapper script preloading a Lisp implementation with various dependencies.

  This function takes a single argument which is a list of Lisp
  libraries programs or programs. The desired Lisp implementation
  will load all given derivations and all their dependencies on
  startup.

  The shortcut `buildLisp.sbclWith` for `buildLisp.sbcl.lispWith` is also provided.

* `repl` passthru attribute: `derivation.repl` is provided as a shortcut
  for `buildLisp.${implementationName}.lispWith [ derivation ]`.
  `derivation.ccl.repl`, `derivation.sbcl.repl` etc. work as well, of course
  (see also "Implementations" section).

## Implementations

Both `buildLisp.library` and `buildLisp.program` allow specifying a different
Common Lisp implementation than the default one (which is SBCL). When an
implementation is passed, `buildLisp` makes sure all dependencies are built
with that implementation as well since build artifacts from different
implementation will be incompatible with each other.

The argument taken by `implementation` is a special attribute set which
describes how to do certain tasks for a given implementation, like building
or loading a library. In case you want to use a custom implementation
description, the precise structure needed is documented in `buildLisp`'s
source code for now. `buildLisp` also exposes the following already
working implementation sets:

* `buildLisp.sbcl`: [SBCL][sbcl], our default implementation

* `buildLisp.ccl`: [CCL][ccl], similar to SBCL, but with very good macOS support

* `buildLisp.ecl`: [ECL][ecl] setup to produce statically linked binaries and
  libraries. Note that its runtime library is LGPL, so [extra conditions][lgpl-static]
  must be fulfilled when distributing binaries produced this way.

* Support for ABCL is planned.

For every of these “known” implementations, `buildLisp` will create a `passthru`
attribute named like the implementation which points to a variant of the derivation
built with said implementation. Say we have a derivation, `myDrv`, built using SBCL:
While `myDrv` and `myDrv.sbcl` are built using SBCL, `myDrv.ecl`, `myDrv.ccl` etc.
build the derivation and all its dependencies using ECL and CCL respectively.

This is useful to test portability of your derivation, but is also used internally
to speed up the “normalization” of the dependency graph. Thus it is important to
make sure that your custom implementation's name doesn't clash with one of the
“known” ones.

## Handling Implementation Specifics

When targeting multiple Common Lisp implementation, it is often necessary to
handle differing interfaces for OS interaction or to make use of special
implementation features. For this reason, `buildLisp` allows specifying
dependencies and source files for specific implementations only. This can
be utilized by having an attribute set in the list for the `deps` or `srcs`
argument: `buildLisp` will pick the value of the attribute named like the
used implementation or `default` and ignore the set completely if both
are missing.

```nix
{ buildLisp, lispPkgs }:

buildLisp.library {
  name = "mylib";

  srcs = [
    # These are included always of course
    ./package.lisp
    ./portable-lib.lisp

    # Choose right impl-* file
    {
      sbcl = ./impl-sbcl.lisp;
      ccl = ./impl-ccl.lisp;
      ecl = ./impl-ecl.lisp;
    }

    # We can also use this to inject extra files
    { ecl = ./extra-ecl-optimizations.lisp; }
  ];

  deps = [
    # Use SBCL's special bundled package, flexi-streams otherwise
    {
      sbcl = buildLisp.bundled "sb-rotate-byte";
      default = lispPkgs.flexi-streams;
    }
  ];
}
```

Additionally a `brokenOn` parameter is accepted which takes a list of
implementation names on which the derivation is not expected to work.
This only influences `meta.targets` which is read by depot's CI to
check which variants (see "Implementations") of the derivation to
build, so it may not be useful outside of depot.

[sbcl]: http://www.sbcl.org/
[ccl]: https://ccl.clozure.com/
[ecl]: https://common-lisp.net/project/ecl/
[lgpl-static]: https://www.gnu.org/licenses/gpl-faq.en.html#LGPLStaticVsDynamic
