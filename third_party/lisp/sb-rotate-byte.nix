{ depot, pkgs, ... }:

let

  src = pkgs.fetchFromGitHub {
    owner = "sbcl";
    repo = "sbcl";
    rev = "0dfedfcfeac04c81255f1df07ecaa71277957d0b";
    sha256 = "00agbacnn2ykda4rm5z9b5f26f8cirajlg71rg94g2msys4mafqx";
  };

in depot.nix.buildLisp.library {
  name = "sb-rotate-byte";

  srcs = map (f: src + ("/contrib/sb-rotate-byte/" + f)) [
    "package.lisp"
    "compiler.lisp"
    "x86-64-vm.lisp"
    "rotate-byte.lisp"
  ];
}
