{ depot, pkgs, ... }:

let
  src = with pkgs; srcOnly lispPackages.anaphora;
in
depot.nix.buildLisp.library {
  name = "anaphora";

  srcs = map (f: src + ("/" + f)) [
    "packages.lisp"
    "early.lisp"
    "symbolic.lisp"
    "anaphora.lisp"
  ];
}
