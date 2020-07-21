{ depot, pkgs, ... }:

let

  src = pkgs.fetchFromGitHub {
    owner = "edicl";
    repo = "cl-who";
    rev = "0d3826475133271ee8c590937136c1bc41b8cbe0";
    sha256 = "0sc8nji9q1df04lhsiwsjy1a35996bibl31w5hp5sh8q6sa122dy";
  };

in depot.nix.buildLisp.library {
  name = "cl-who";

  deps = with depot.third_party.lisp; [
    alexandria
    lisp-namespace
    closer-mop
    trivial-cltl2
  ];

  srcs = map (f: src + ("/" + f)) [
    "packages.lisp"
    "specials.lisp"
    "util.lisp"
    "who.lisp"
  ];
}
