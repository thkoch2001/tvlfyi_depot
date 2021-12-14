{ depot, pkgs, ... }:

let
  inherit (depot.nix.buildLisp) bundled;
  src = with pkgs; srcOnly lispPackages.nibbles;
in depot.nix.buildLisp.library {
  name = "nibbles";

  deps = with depot.third_party.lisp; [
    (bundled "asdf")
  ];

  srcs = map (f: src + ("/" + f)) [
    "package.lisp"
    "types.lisp"
    "macro-utils.lisp"
    "vectors.lisp"
    "streams.lisp"
  ] ++ [
    { sbcl = "${src}/sbcl-opt/fndb.lisp"; }
    { sbcl = "${src}/sbcl-opt/nib-tran.lisp"; }
    { sbcl = "${src}/sbcl-opt/x86-vm.lisp"; }
    { sbcl = "${src}/sbcl-opt/x86-64-vm.lisp"; }
  ];
}
