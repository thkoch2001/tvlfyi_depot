# A Common Lisp abstraction layer over platform dependent functionality.
{ depot, pkgs, ... }:

let src = with pkgs; srcOnly lispPackages.s-sysdeps;
in depot.nix.buildLisp.library {
  name = "s-sysdeps";

  srcs = [
    "${src}/src/package.lisp"
    "${src}/src/sysdeps.lisp"
  ];
}
