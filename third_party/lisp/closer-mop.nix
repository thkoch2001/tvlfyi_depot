# Closer to MOP is a compatibility layer that rectifies many of the
# absent or incorrect CLOS MOP features across a broad range of Common
# Lisp implementations
{ depot, pkgs, ... }:

let src = with pkgs; srcOnly lispPackages.closer-mop;
in depot.nix.buildLisp.library {
  name = "closer-mop";

  srcs = [
    "${src}/closer-mop-packages.lisp"
    "${src}/closer-mop-shared.lisp"
    {
      sbcl = "${src}/closer-sbcl.lisp";
      ecl = "${src}/closer-ecl.lisp";
      ccl = "${src}/closer-clozure.lisp";
    }
  ];

  brokenOn = [
    "ecl" # https://github.com/pcostanza/closer-mop/issues/20
  ];
}
