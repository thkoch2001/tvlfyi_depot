{ depot, pkgs, ... }:

let src = with pkgs; srcOnly lispPackages.parseq;
in depot.nix.buildLisp.library {
  name = "parseq";

  srcs = map (f: src + ("/" + f)) [
    "package.lisp"
    "conditions.lisp"
    "utils.lisp"
    "defrule.lisp"
  ];
}
