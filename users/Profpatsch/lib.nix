{ depot, pkgs, ... }:
let
  bins = depot.nix.getBins pkgs.coreutils ["printf" "echo"];

  debugExec = msg: depot.nix.writeExecline "debug-exec" {} [
    "if" [
      "fdmove" "-c" "1" "2"
      "if" [ bins.printf "%s: " msg ]
      "if" [ bins.echo "$@" ]
    ]
    "$@"
  ];

  eprintf = depot.nix.writeExecline "eprintf" {} [
    "fdmove" "-c" "1" "2" bins.printf "%s" "$@"
  ];

in {
  inherit
    debugExec
    eprintf
    ;
}
