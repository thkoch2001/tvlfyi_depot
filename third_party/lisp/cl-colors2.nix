{ depot, pkgs, ... }:

let src = with pkgs; srcOnly lispPackages.cl-colors2;
in depot.nix.buildLisp.library {
  name = "cl-colors2";
  deps = with depot.third_party.lisp; [
    alexandria
    cl-ppcre
  ];

  srcs = map (f: src + ("/" + f)) [
    "package.lisp"
    "colors.lisp"
    "colornames.lisp"
    "hexcolors.lisp"
  ];
}
