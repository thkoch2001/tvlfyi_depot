# Library for manipulating dates & times
{ depot, pkgs, ... }:

let src = with pkgs; srcOnly lispPackages.local-time;
in depot.nix.buildLisp.library {
  name = "local-time";
  deps = [
    (depot.nix.buildLisp.bundled "uiop")
    depot.third_party.lisp.cl-fad
  ];

  srcs = [
    "${src}/src/package.lisp"
    "${src}/src/local-time.lisp"
  ];
}
