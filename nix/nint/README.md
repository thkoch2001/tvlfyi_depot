# nint — Nix INTerpreter

`nint` is a shebang compatible interpreter for nix. It is currently
implemented as a fairly trivial wrapper around `nix-instantiate --eval`.
It allows to run nix expressions as command line tools if they conform
to the following calling convention:

* Every nix script needs to evaluate to a function which takes an
  attribute set as its single argument. Ideally a set pattern with
  an ellipsis should be used. By default `nint` passes the following
  arguments:

  * `currentDir`: the current working directory as a nix path
  * `argv`: a list of arguments to the invokation including the
    program name at `builtins.head argv`.
  * Extra arguments can be manually passed as described below.

* The return value should always be a string (throwing is also okay)
  which is printed to stdout by `nint`.

## Usage

```
nint [ --arg ARG VALUE … ] script.nix [ ARGS … ]
```

Instead of `--arg`, `--argstr` can also be used. They both work
like the flags of the same name for `nix-instantiate` and may
be specified any number of times as long as they are passed
*before* the nix expression to run.

Below is a shebang which also passes `depot` as an argument
(note the usage of `env -S` to get around the shebang limitation
to two arguments).

```nix
#!/usr/bin/env -S nint --arg depot /path/to/depot
```

## Limitations

* No side effects except for writing to `stdout`.

* Output is not streaming, i. e. even if the output is incrementally
  calculated, nothing will be printed until the full output is available.
  With plain nix strings we can't do better anyways.

* Limited error handling for the script, no way to set the exit code etc.

Some of these limitations may be possible to address in the future by using
an alternative nix interpreter and a more elaborate calling convention.

## Example

Below is a (very simple) implementation of a `ls(1)`-like program in nix:

```nix
#!/usr/bin/env nint
{ currentDir, argv, ... }:

let
  lib = import <nixpkgs/lib>;

  dirs =
    let
      args = builtins.tail argv;
    in
      if args == []
      then [ currentDir ]
      else args;

  makeAbsolute = p:
    if builtins.isPath p
    then p
    else if builtins.match "^/.*" p != null
    then p
    else "${toString currentDir}/${p}";
in

  lib.concatStringsSep "\n"
    (lib.flatten
      (builtins.map
        (d: (builtins.attrNames (builtins.readDir (makeAbsolute d))))
        dirs)) + "\n"
```
