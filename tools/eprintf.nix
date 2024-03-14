{ depot, pkgs, ... }:

let
  bins = depot.nix.getBins pkgs.coreutils [ "printf" ];
in
# printf(1), but redirect to stderr
depot.nix.writeExecline "eprintf" { } [
  "fdmove"
  "-c"
  "1"
  "2"
  bins.printf
  "$@"
]
