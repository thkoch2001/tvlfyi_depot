# This library is meant to make writing portable multi-threaded apps
# in Common Lisp simple.
{ depot, pkgs, ... }:

let
  src = with pkgs; srcOnly lispPackages.bordeaux-threads;
  getSrc = f: "${src}/src/${f}";
in
depot.nix.buildLisp.library {
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
