# Portability library for CL gray streams.
{ depot, pkgs, ... }:

let
  src = with pkgs; srcOnly lispPackages.trivial-gray-streams;
in
depot.nix.buildLisp.library {
  name = "trivial-gray-streams";
  srcs = [
    (src + "/package.lisp")
    (src + "/streams.lisp")
  ];
}
