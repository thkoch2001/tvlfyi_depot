# Compatibility layer for minor MOP implementation differences
{ depot, pkgs, ... }:

let src = with pkgs; srcOnly sbclPackages.moptilities;
in depot.nix.buildLisp.library {
  name = "moptilities";
  deps = [ depot.third_party.lisp.closer-mop ];
  srcs = [ "${src}/dev/moptilities.lisp" ];

  brokenOn = [
    "ecl" # TODO(sterni): https://gitlab.com/embeddable-common-lisp/ecl/-/issues/651
  ];
}
