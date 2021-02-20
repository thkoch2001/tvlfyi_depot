# nint — Nix INTerpreter

`nint` is a shebang compatible interpreter for nix. It is currently
implemented as a fairly trivial wrapper around `nix-instantiate --eval`.
It allows to run nix expressions which take an attribute set as an input
and return a string as command line scripts. This is achieved by the
following calling convention:

* Every nix script needs to evaluate to a function which takes an
  attribute set as the single argument. Ideally a set pattern with
  an ellipsis should be used. By default `nint` passes the following
  arguments:

  * `currentDir`: the current working directory as a nix path
  * `argv`: a list of arguments to the invokation including the
    program name at `builtins.head argv`.
  * Extra arguments can be manually passed as described below.

* The return value should always be string (on success at least)
  which is printed to stdout by the wrapper.

## Usage

```
nint [ --arg ARG VALUE … ] script.nix [ ARGS … ]
```

Instead of `--arg`, `--argstr` can also be used, they both work
like the same named flags for `nix-instantiate` and can also
be passed multiple times. It is important however that they
are passed *before* the path to the script to be executed.

## Limitations

* No side effects except for writing to `stdout`.

* Output is not streaming, i. e. even if the output is incrementally
  calculated, nothing will be printed until the full output is available.

* Limited error handling for the script, no way to set exit code etc.

Some of these limitations may be possible to address in the future by using
an alternative nix interpreter and a more elaborate calling convention.
