{ depot, pkgs, ... }:

let
  inherit (depot.nix.buildLisp) bundled;
  src = pkgs.fetchFromGitHub {
    owner = "sharplispers";
    repo = "nibbles";
    rev = "dad25240928d5cf8f7df69c4398244e03570bb35";
    sha256 = "0r6ljlpgjmkf87pmvdwzva8qj15bhznc3ylgcjjqyy4frbx9lygz";
    name = "nibbles-source";
  };

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
