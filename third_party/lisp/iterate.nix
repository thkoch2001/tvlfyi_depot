# iterate is an iteration construct for Common Lisp, similar to the
# LOOP macro.
{ depot, pkgs, ... }:

let src = with pkgs; srcOnly sbclPackages.iterate;
in depot.nix.buildLisp.library {
  name = "iterate";
  srcs = [
    "${src}/package.lisp"
    "${src}/iterate.lisp"
  ];
}
