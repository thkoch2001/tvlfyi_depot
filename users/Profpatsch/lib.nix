{ depot, pkgs, ... }:
let
  bins = depot.nix.getBins pkgs.coreutils [ "printf" "echo" "cat" "printenv" ]
      // depot.nix.getBins pkgs.fdtools [ "multitee" ]
      ;

  debugExec = msg: depot.nix.writeExecline "debug-exec" {} [
    "if" [
      "fdmove" "-c" "1" "2"
      "if" [ bins.printf "%s: " msg ]
      "if" [ bins.echo "$@" ]
    ]
    "$@"
  ];

  eprintf = depot.nix.writeExecline "eprintf" {} [
    "fdmove" "-c" "1" "2" bins.printf "$@"
  ];

  eprint-stdin = depot.nix.writeExecline "eprint-stdin" {} [
    "pipeline" [ bins.multitee "0-1,2" ] "$@"
  ];

  eprintenv = depot.nix.writeExecline "eprintenv" { readNArgs = 1; } [
    "fdmove" "-c" "1" "2" bins.printenv "$1"
  ];

in {
  inherit
    debugExec
    eprintf
    eprint-stdin
    eprintenv
    ;
}
