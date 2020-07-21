{ depot, pkgs, ... }:

let

  src = pkgs.fetchFromGitHub {
    owner = "guicho271828";
    repo = "trivia";
    rev = "37698b47a14c2007630468de7a993694ef7bd475";
    sha256 = "0rsbwbw3ipxxgr6zzhci12nilq8zky475kmhz1rcxy4q8a85vn72";
  };

in depot.nix.buildLisp.library {
  name = "trivia.trivial";

  deps = with depot.third_party.lisp; [
    alexandria
    lisp-namespace
    closer-mop
    trivial-cltl2
  ];

  srcs = map (f: src + ("/" + f)) [
    "level0/package.lisp"
    "level0/impl.lisp"

    "level1/package.lisp"
    "level1/impl.lisp"

    "level2/package.lisp"
    "level2/impl.lisp"
    "level2/sugars.lisp"
    "level2/derived.lisp"
    "level2/derived-class.lisp"
    "level2/derived2.lisp"
    "level2/derived3.lisp"
    "level2/derived-numbers.lisp"
    "level2/arrays.lisp"
    "level2/inline-pattern.lisp"
  ];
}
