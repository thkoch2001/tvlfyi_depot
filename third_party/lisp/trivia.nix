{ depot, pkgs, ... }:

let

  src = pkgs.fetchFromGitHub {
    owner = "guicho271828";
    repo = "trivia";
    rev = "37698b47a14c2007630468de7a993694ef7bd475";
    sha256 = "0rsbwbw3ipxxgr6zzhci12nilq8zky475kmhz1rcxy4q8a85vn72";
  };

in depot.nix.buildLisp.library {
  name = "trivia";

  deps = with depot.third_party.lisp; [
    trivia-trivial
    type-i
    iterate
    alexandria
  ];

  srcs = map (f: src + ("/" + f)) [
    "balland2006/package.lisp"
    "balland2006/optimizer.lisp"
    "balland2006/column-swapping.lisp"
  ];
}
