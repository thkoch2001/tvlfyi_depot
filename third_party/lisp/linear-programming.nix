{ depot ? import <depot> {}, ... }:

let
  src = builtins.fetchGit {
    url = "https://github.com/neil-lindquist/linear-programming.git";
    rev = "8c8d55e7584773b90c4ba4b225c5f2008f4c474a";
  };
in depot.nix.buildLisp.library {
  name = "linear-programming";
  deps = [
    (depot.nix.buildLisp.bundled "uiop")
    depot.third_party.lisp.iterate
    depot.third_party.lisp.alexandria
  ];
  srcs = [
    "${src}/src/conditions.lisp"
    "${src}/src/expressions.lisp"
    "${src}/src/simplex.lisp"
    "${src}/src/system-info.lisp"
    "${src}/src/utils.lisp"
    "${src}/src/problem.lisp"
    "${src}/src/solver.lisp"
    "${src}/src/external-formats.lisp"
    "${src}/src/all.lisp"
  ];
}
