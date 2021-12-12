{ depot, pkgs, ... }:

let
  src = pkgs.fetchFromGitHub {
    owner = "tokenrove";
    repo = "anaphora";
    rev = "018590df36ffb30ece561fb28ea6521363efc6e2";
    sha256 = "0pq6y5swvrjd0kjs2dl2648s13s0pzxin0chrq35jam8jrci3kd1";
  };
in depot.nix.buildLisp.library {
  name = "anaphora";

  srcs = map (f: src + ("/" + f)) [
    "packages.lisp"
    "early.lisp"
    "symbolic.lisp"
    "anaphora.lisp"
  ];
}
