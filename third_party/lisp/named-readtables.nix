{ depot, pkgs, ... }:

let
  src = pkgs.fetchFromGitHub {
    owner = "melisgl";
    repo = "named-readtables";
    rev = "64bd53f37a1694cfde48fc38b8f03901f6f0c05b";
    sha256 = "01l4831m7k84qvhzyx0qgdl50isr4zmp40qf6dfq2iqcaj8y4h3n";
  };
in depot.nix.buildLisp.library {
  name = "named-readtables";

  srcs = map (f: src + ("/src/" + f)) [
    "package.lisp"
    "utils.lisp"
    "define-api.lisp"
    "cruft.lisp"
    "named-readtables.lisp"
  ];
}
