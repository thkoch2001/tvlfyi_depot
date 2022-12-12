# A library to easily read and write complex binary formats.
{ depot, pkgs, ... }:

let
  src = pkgs.fetchFromGitHub {
    owner = "j3pic";
    repo = "lisp-binary";
    rev = "711c38c2d9862bec7b83c8bf42823dd3985dc517";
    sha256 = "0vn1kjvcch9ky50rq1axg5hixf3zkbb46as99g0aks1b7y250a17";
  };
in
depot.nix.buildLisp.library {
  name = "lisp-binary";

  deps = with depot.third_party.lisp; [
    cffi
    alexandria
    quasiquote_2
    moptilities
    flexi-streams
    closer-mop
  ];

  srcs = map (f: src + ("/" + f)) [
    "utils.lisp"
    "integer.lisp"
    "float.lisp"
    "simple-bit-stream.lisp"
    "reverse-stream.lisp"
    "binary-1.lisp"
    "binary-2.lisp"
    "types.lisp"
  ];

  brokenOn = [
    "ecl" # TODO(sterni): disable conditionally cffi for ECL
  ];
}
