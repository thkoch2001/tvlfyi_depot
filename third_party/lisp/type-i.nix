{ depot, pkgs, ... }:

let

  src = pkgs.fetchFromGitHub {
    owner = "guicho271828";
    repo = "type-i";
    rev = "d34440ab4ebf5a46a58deccb35950b15670e3667";
    sha256 = "12wsga0pwjkkr176lnjwkmmlm3ccp0n310sjj9h20lk53iyd0z69";
  };

in depot.nix.buildLisp.library {
  name = "type-i";

  deps = with depot.third_party.lisp; [
    introspect-environment
    alexandria
    trivia-trivial
    lisp-namespace
  ];

  srcs = map (f: src + ("/src/" + f)) [
    "package.lisp"
    "infer-typep.lisp"
    "infer-unary.lisp"
    "infer-derived.lisp"
    "infer-constants.lisp"
    "infer-compound.lisp"
    "infer-numbers.lisp"
  ];
}
