{ tpkgs ? (import (builtins.fetchGit "https://git.tazj.in/") {}), ... }:

let
  src = builtins.fetchGit {
    url = "https://github.com/fukamachi/prove.git";
    rev = "5d71f02795b89e36f34e8c7d50e69b67ec6ca2de";
  };
in tpkgs.nix.buildLisp.library {
  name = "prove-asdf";
  deps = [];
  srcs = [
    "${src}/src/output.lisp"
    "${src}/src/asdf.lisp"
  ];
}
