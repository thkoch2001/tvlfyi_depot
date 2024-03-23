{ depot, pkgs, ... }:

let src = with pkgs; srcOnly lispPackages.cl-change-case;
in depot.nix.buildLisp.library {
  name = "cl-change-case";

  deps = with depot.third_party.lisp; [ cl-ppcre cl-ppcre.unicode ];

  srcs = [ (src + "/src/cl-change-case.lisp") ];
}
