{ depot, pkgs, ... }:

let

  src = pkgs.fetchFromGitHub {
    owner = "sbcl";
    repo = "sbcl";
    rev = "0dfedfcfeac04c81255f1df07ecaa71277957d0b";
    sha256 = "00agbacnn2ykda4rm5z9b5f26f8cirajlg71rg94g2msys4mafqx";
  };

in depot.nix.buildLisp.library {
  name = "sb-cltl2";

  srcs = map (f: src + ("/contrib/sb-cltl2/" + f)) [
    "defpackage.lisp"
    "compiler-let.lisp"
    "macroexpand.lisp"
    "env.lisp"
  ];
}
