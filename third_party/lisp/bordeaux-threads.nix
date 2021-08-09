# This library is meant to make writing portable multi-threaded apps
# in Common Lisp simple.
{ depot, ... }:

let
  src = builtins.fetchGit {
    url = "https://github.com/sionescu/bordeaux-threads.git";
    rev = "499b6d3f0ce635417d6096acf0a671d8bf3f6e5f";
  };
  getSrc = f: "${src}/src/${f}";
in depot.nix.buildLisp.library {
  name = "bordeaux-threads";
  deps = [ depot.third_party.lisp.alexandria ];

  srcs = map getSrc [
    "pkgdcl.lisp"
    "bordeaux-threads.lisp"
  ] ++ [
    {
      sbcl = getSrc "impl-sbcl.lisp";
      ecl = getSrc "impl-ecl.lisp";
    }
  ] ++ map getSrc [
    "default-implementations.lisp"
  ];
}
