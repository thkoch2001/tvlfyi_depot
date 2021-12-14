{ depot, pkgs, ... }:

let
  src = pkgs.fetchgit {
    url = "https://github.com/tpapp/let-plus.git";
    rev = "7cf18b29ed0fe9c667a9a6a101b08ab9661a59e9";
    hash = "sha256:1xy3b05dwjddk33mah2jaigm4jzsmrxjcc1d0dhsw0krwgr4450f";
  };
in depot.nix.buildLisp.library {
  name = "let-plus";
  deps = [
    depot.third_party.lisp.alexandria
    depot.third_party.lisp.anaphora
  ];
  srcs = [
    "${src}/package.lisp"
    "${src}/let-plus.lisp"
    "${src}/extensions.lisp"
  ];
}
