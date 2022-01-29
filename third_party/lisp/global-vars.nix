{ depot, pkgs, ... }:

let src = with pkgs; srcOnly lispPackages.global-vars;
in
depot.nix.buildLisp.library {
  name = "global-vars";
  srcs = [ "${src}/global-vars.lisp" ];
}
