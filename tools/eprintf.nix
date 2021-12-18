{ depot, pkgs, ... }:

let
  bins = depot.nix.getBins pkgs.coreutils [ "printf" ];

  # printf(1), but redirect to stderr
in
depot.nix.writeExecline "eprintf" { } [
  "fdmove"
  "-c"
  "1"
  "2"
  bins.printf
  "$@"
]
