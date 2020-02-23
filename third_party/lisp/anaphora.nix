{ depot, ... }:

let
  src = builtins.fetchGit {
    url = "https://github.com/tokenrove/anaphora.git";
    rev = "aeace4c68cf55098a67112750b28f8f2dc6d0e30";
  };
in depot.nix.buildLisp.library {
  name = "anaphora";
  deps = [];
  srcs = [
    "${src}/packages.lisp"
    "${src}/early.lisp"
    "${src}/symbolic.lisp"
    "${src}/anaphora.lisp"
  ];
}
