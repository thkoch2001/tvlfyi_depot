{ depot, pkgs, ... }:

let src = with pkgs; srcOnly lispPackages.cl-utilities;

in depot.nix.buildLisp.library {
  name = "cl-utilities";

  srcs = map (f: src + "/" + f) [
    "package.lisp"
    "split-sequence.lisp"
    "with-unique-names.lisp"
    "once-only.lisp"
    "extremum.lisp"
    "read-delimited.lisp"
    "expt-mod.lisp"
    "compose.lisp"
    "collecting.lisp"
    "rotate-byte.lisp"
    "copy-array.lisp"
  ];
}
