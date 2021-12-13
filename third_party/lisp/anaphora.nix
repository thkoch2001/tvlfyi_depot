<<<<<<< HEAD   (464bbc feat(sterni/aoc/2021): day 9 solution)
{ depot, pkgs, ... }:

let src = pkgs.fetchFromGitHub {
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
=======
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
>>>>>>> BRANCH (6123e9 playbooks: add hip_opening_challenge)
  ];
}
