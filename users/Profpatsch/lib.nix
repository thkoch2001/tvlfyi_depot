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
    "fdmove" "-c" "1" "2" bins.printenv "$1" "$@"
  ];

  # remove everything but a few selected environment variables
  runInEmptyEnv = keepVars:
    let
        importas = pkgs.lib.concatMap (var: [ "importas" "-i" var var ]) keepVars;
        # we have to explicitely call export here, because PATH is probably empty
        export = pkgs.lib.concatMap (var: [ "${pkgs.execline}/bin/export" var ''''${${var}}'' ]) keepVars;
    in depot.nix.writeExecline "empty-env" {}
         (importas ++ [ "emptyenv" ] ++ export ++ [ "${pkgs.execline}/bin/exec" "$@" ]);


in {
  inherit
    debugExec
    eprintf
    eprint-stdin
    eprintenv
    runInEmptyEnv
    ;
}
