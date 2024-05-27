{ depot, pkgs, ... }:

let src = with pkgs; srcOnly sbclPackages.parse-number;
in depot.nix.buildLisp.library {
  name = "parse-number";
  srcs = map (f: src + ("/" + f)) [
    "parse-number.lisp"
  ];
}
