{ depot, pkgs, ... }:

let src = with pkgs; srcOnly sbclPackages.global-vars;
in depot.nix.buildLisp.library {
  name = "global-vars";
  srcs = [ "${src}/global-vars.lisp" ];
}
