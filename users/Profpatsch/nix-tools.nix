{ depot, pkgs, ... }:

let
  bins = depot.nix.getBins pkgs.nix [ "nix-build" "nix-instantiate" ];

  # TODO: both of these don’t prevent `result` from being created. good? bad?

  # Usage (execline syntax):
  #    nix-run { -A foo <more_nix_options> } args...
  #
  # Takes an execline block of `nix-build` arguments, which should produce an executable store path.
  # Then runs the store path with `prog...`.
  nix-run = depot.nix.writeExecline "nix-run" { argMode = "env"; } [
    "backtick"
    "-iE"
    "storepath"
    [
      runblock
      "1"
      bins.nix-build
    ]
    runblock
    "-r"
    "2"
    "$storepath"
  ];

  # Usage (execline syntax):
  #    nix-run-bin { -A foo <more_nix_options> } <foo_bin_name> args...
  #
  # Takes an execline block of `nix-build` arguments, which should produce a store path with a bin/ directory in it.
  # Then runs the given command line with the given arguments. All executables in the built storepath’s bin directory are prepended to `PATH`.
  nix-run-bin = depot.nix.writeExecline "nix-run-bin" { argMode = "env"; } [
    "backtick"
    "-iE"
    "storepath"
    [
      runblock
      "1"
      bins.nix-build
    ]
    "importas"
    "-ui"
    "PATH"
    "PATH"
    "export"
    "PATH"
    "\${storepath}/bin:\${PATH}"
    runblock
    "-r"
    "2"
  ];

  nix-eval = depot.nix.writeExecline "nix-eval" { } [
    bins.nix-instantiate
    "--read-write-mode"
    "--eval"
    "--strict"
    "$@"
  ];

  # This is a rewrite of execline’s runblock.
  # It adds the feature that instead of just
  # executing the block it reads, it can also
  # pass it as argv to given commands.
  #
  # This is going to be added to a future version
  # of execline by skarnet, but for now it’s easier
  # to just dirtily reimplement it in Python.
  #
  # TODO: this was added to recent execline versions,
  # but it doesn’t seem to be a drop-in replacement,
  # if I use execline’s runblock in nix-run-bin above,
  # I get errors like
  # > export: fatal: unable to exec runblock: Success
  runblock = pkgs.writers.writePython3 "runblock"
    {
      flakeIgnore = [ "E501" "E226" ];
    } ''
    import sys
    import os
    from pathlib import Path

    skip = False
    one = sys.argv[1]
    if one == "-r":
        skip = True
        block_number = int(sys.argv[2])
        block_start = 3
    elif one.startswith("-"):
        print("runblock-python: only -r supported", file=sys.stderr)
        sys.exit(100)
    else:
        block_number = int(one)
        block_start = 2

    execline_argv_no = int(os.getenvb(b"#"))
    runblock_argv = [os.getenv(str(no)) for no in range(1, execline_argv_no + 1)]


    def parse_block(args):
        new_args = []
        if args == []:
            print(
                "runblock-python: empty block",
                file=sys.stderr
            )
            sys.exit(100)
        for arg in args:
            if arg == "":
                break
            elif arg.startswith(" "):
                new_args.append(arg[1:])
            else:
                print(
                    "runblock-python: unterminated block: {}".format(args),
                    file=sys.stderr
                )
                sys.exit(100)
        args_rest = args[len(new_args)+1:]
        return (new_args, args_rest)


    if skip:
        rest = runblock_argv
        for _ in range(0, block_number-1):
            (_, rest) = parse_block(rest)
        new_argv = rest
    else:
        new_argv = []
        rest = runblock_argv
        for _ in range(0, block_number):
            (new_argv, rest) = parse_block(rest)

    given_argv = sys.argv[block_start:]
    run = given_argv + new_argv
    if os.path.isabs(run[0]):
        # TODO: ideally I’d check if it’s an executable here, but it was too hard to figure out and I couldn’t be bothered tbh
        if not Path(run[0]).is_file():
            print(
                "runblock-python: Executable {} does not exist or is not a file.".format(run[0]),
                file=sys.stderr
            )
            sys.exit(100)
    os.execvp(
        file=run[0],
        args=run
    )
  '';


in
{
  inherit
    nix-run
    nix-run-bin
    nix-eval
    ;
}
