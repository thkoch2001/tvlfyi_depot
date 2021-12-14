{ depot, pkgs, ... }:

let src = with pkgs; srcOnly lispPackages.marshal;
in depot.nix.buildLisp.library {
  name = "marshal";
  srcs = map (f: src + ("/" + f)) [
    "package.lisp"
    "serialization-format.lisp"
    "coding-idiom.lisp"
    "marshal.lisp"
    "unmarshal.lisp"
  ];
}
