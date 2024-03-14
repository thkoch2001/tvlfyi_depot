{ ... }:

let
  /*
    Returns true if it is being evaluated using restrict-eval, false if not.
    It's more robust than using `builtins.getEnv` since it isn't fooled by
    `env -i`.

    See https://github.com/NixOS/nix/issues/6579 for a description of the
    behavior. Precise cause in the evaluator / store implementation is unclear.

    Type: bool
  */
  inRestrictedEval = builtins.pathExists (toString ./guinea-pig + "/.");
in

{
  inherit inRestrictedEval;
}
