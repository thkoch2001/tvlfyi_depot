# CFFI purports to be the Common Foreign Function Interface.
{ depot, ... }:

with depot.nix;
let src = builtins.fetchGit {
  url = "https://github.com/cffi/cffi.git";
  rev = "a49ff36a95cb62ffa6cb069d98378d665769926b";
};
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
