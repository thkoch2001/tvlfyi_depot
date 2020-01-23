{ tpkgs ? (import (builtins.fetchGit "https://git.tazj.in/") {}), ... }:

let
  src = builtins.fetchGit {
    url = "https://github.com/tpapp/cl-colors.git";
    rev = "827410584553f5c717eec6182343b7605f707f75";
  };
in tpkgs.nix.buildLisp.library {
  name = "cl-colors";
  deps = with tpkgs.third_party.lisp; [
    alexandria
    (import ./let-plus.nix {})
  ];
  srcs = [
    "${src}/package.lisp"
    "${src}/colors.lisp"
    "${src}/colornames.lisp"
    "${src}/hexcolors.lisp"
  ];
}
