{ tpkgs ? (import (builtins.fetchGit "https://git.tazj.in/") {}), ... }:

let
  src = builtins.fetchGit {
    url = "https://github.com/tpapp/let-plus.git";
    rev = "7cf18b29ed0fe9c667a9a6a101b08ab9661a59e9";
  };
in tpkgs.nix.buildLisp.library {
  name = "let-plus";
  deps = with tpkgs.third_party.lisp; [
    alexandria
    (import ./anaphora.nix {})
  ];
  srcs = [
    "${src}/package.lisp"
    "${src}/let-plus.lisp"
    "${src}/extensions.lisp"
  ];
}
