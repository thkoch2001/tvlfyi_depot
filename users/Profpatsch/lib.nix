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

  eprint-stdin = depot.nix.writeExecline "eprint-stdin" {} [
    "pipeline" [ bins.multitee "0-1,2" ] "$@"
  ];

  eprint-stdin-netencode = depot.nix.writeExecline "eprint-stdin-netencode" {} [
    "pipeline" [
      # move stdout to 3
      "fdmove" "3" "1"
      # the multitee copies stdin to 1 (the other pipeline end) and 3 (the stdout of the outer pipeline block)
      "pipeline" [ bins.multitee "0-1,3" ]
      # make stderr the stdout of pretty, merging with the stderr of pretty
      "fdmove" "-c" "1" "2"
      depot.users.Profpatsch.netencode.pretty
    ]
    "$@"
  ];

  eprintenv = depot.nix.writeExecline "eprintenv" { readNArgs = 1; } [
    "ifelse" [ "fdmove" "-c" "1" "2" bins.printenv "$1" ]
    [ "$@" ]
    "if" [ depot.tools.eprintf "eprintenv: could not find \"\${1}\" in the environment\n" ]
    "$@"
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
    eprint-stdin
    eprint-stdin-netencode
    eprintenv
    runInEmptyEnv
    ;
}
