{ depot, pkgs, ... }:
let
  bins = depot.nix.getBins pkgs.coreutils [ "printf" "echo" "cat" "printenv" "tee" ]
    // depot.nix.getBins pkgs.bash [ "bash" ]
    // depot.nix.getBins pkgs.fdtools [ "multitee" ]
  ;

  # Print `msg` and and argv to stderr, then execute into argv
  debugExec = msg: depot.nix.writeExecline "debug-exec" { } [
    "if"
    [
      "fdmove"
      "-c"
      "1"
      "2"
      "if"
      [ bins.printf "%s: " msg ]
      "if"
      [ bins.echo "$@" ]
    ]
    "$@"
  ];

  # Print stdin to stderr and stdout
  eprint-stdin = depot.nix.writeExecline "eprint-stdin" { } [
    "pipeline"
    [ bins.multitee "0-1,2" ]
    "$@"
  ];

  # Assume the input on stdin is netencode, pretty print it to stderr and forward it to stdout
  eprint-stdin-netencode = depot.nix.writeExecline "eprint-stdin-netencode" { } [
    "pipeline"
    [
      # move stdout to 3
      "fdmove"
      "3"
      "1"
      # the multitee copies stdin to 1 (the other pipeline end) and 3 (the stdout of the outer pipeline block)
      "pipeline"
      [ bins.multitee "0-1,3" ]
      # make stderr the stdout of pretty, merging with the stderr of pretty
      "fdmove"
      "-c"
      "1"
      "2"
      depot.users.Profpatsch.netencode.pretty
    ]
    "$@"
  ];

  # print the given environment variable in $1 to stderr, then execute into the rest of argv
  eprintenv = depot.nix.writeExecline "eprintenv" { readNArgs = 1; } [
    "ifelse"
    [ "fdmove" "-c" "1" "2" bins.printenv "$1" ]
    [ "$@" ]
    "if"
    [ depot.tools.eprintf "eprintenv: could not find \"\${1}\" in the environment\n" ]
    "$@"
  ];

  # Split stdin into two commands, given by a block and the rest of argv
  #
  # Example (execline):
  #
  #   pipeline [ echo foo ]
  #   split-stdin [ fdmove 1 2 foreground [ cat ] echo "bar" ] cat
  #
  #   stdout: foo\n
  #   stderr: foo\nbar\n
  split-stdin = depot.nix.writeExecline "split-stdin" { argMode = "env"; } [
    "pipeline"
    [
      # this is horrible yes but the quickest way I knew how to implement it
      "runblock"
      "1"
      bins.bash
      "-c"
      ''${bins.tee} >("$@")''
      "bash-split-stdin"
    ]
    "runblock"
    "-r"
    "1"
  ];

  # remove everything but a few selected environment variables
  runInEmptyEnv = keepVars:
    let
      importas = pkgs.lib.concatMap (var: [ "importas" "-i" var var ]) keepVars;
      # we have to explicitely call export here, because PATH is probably empty
      export = pkgs.lib.concatMap (var: [ "${pkgs.execline}/bin/export" var ''''${${var}}'' ]) keepVars;
    in
    depot.nix.writeExecline "empty-env" { }
      (importas ++ [ "emptyenv" ] ++ export ++ [ "${pkgs.execline}/bin/exec" "$@" ]);


in
{
  inherit
    debugExec
    eprint-stdin
    eprint-stdin-netencode
    eprintenv
    split-stdin
    runInEmptyEnv
    ;
}
