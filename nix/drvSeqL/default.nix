{ depot, lib, pkgs, ... }:

let

  inherit (depot.nix.yants) defun list drv;

  # Realize drvDeps, then return drvOut if that succeds.
  # This can be used to make drvOut depend on the
  # build success of all drvDeps without making each drvDep
  # a dependency of drvOut.
  # => drvOut is not rebuilt if drvDep changes
  drvSeqL = defun [ (list drv) drv drv ] (drvDeps: drvOut:
    let drvOutOutputs = drvOut.outputs or [ "out" ];
    in pkgs.runCommandLocal drvOut.name {
      # we inherit all attributes in order to replicate
      # the original derivation as much as possible
      outputs = drvOutOutputs;
      passthru = drvOut.drvAttrs;
      # depend on drvDeps (by putting it in builder context)
      inherit drvDeps;
    }
    # the outputs of the original derivation are replicated
    # by creating a symlink to the old output path
    (lib.concatMapStrings (output: ''
      target=${lib.escapeShellArg drvOut.${output}}
      # if the target is already a symlink, follow it until it’s not;
      # this is done to prevent too many dereferences
      target=$(readlink -e "$target")
      # link to the output
      ln -s "$target" "${"$"}${output}"
    '') drvOutOutputs));

in { __functor = _: drvSeqL; }
