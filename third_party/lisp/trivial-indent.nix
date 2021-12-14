{ depot, pkgs, ... }:

let src = with pkgs; srcOnly lispPackages.trivial-indent;
in depot.nix.buildLisp.library {
  name = "trivial-indent";

  srcs = map (f: src + ("/" + f)) [
    "indent.lisp"
  ];
}
