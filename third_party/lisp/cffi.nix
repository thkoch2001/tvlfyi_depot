# CFFI purports to be the Common Foreign Function Interface.
{ depot, pkgs, ... }:

with depot.nix;
let src = with pkgs; srcOnly lispPackages.cffi;
in buildLisp.library {
  name = "cffi";
  deps = with depot.third_party.lisp; [
    alexandria
    babel
    trivial-features
    (buildLisp.bundled "asdf")
  ];

  srcs = [
    {
      ecl = src + "/src/cffi-ecl.lisp";
      sbcl = src + "/src/cffi-sbcl.lisp";
      ccl = src + "/src/cffi-openmcl.lisp";
    }
  ] ++ map (f: src + ("/src/" + f)) [
    "package.lisp"
    "utils.lisp"
    "libraries.lisp"
    "early-types.lisp"
    "types.lisp"
    "enum.lisp"
    "strings.lisp"
    "structures.lisp"
    "functions.lisp"
    "foreign-vars.lisp"
    "features.lisp"
  ];
}
