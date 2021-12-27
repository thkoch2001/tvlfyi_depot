# Expose secrets as part of the tree, making it possible to validate
# their paths at eval time.
#
# Note that encrypted secrets end up in the Nix store, but this is
# fine since they're publicly available anyways.
{ depot, lib, ... }:

let
  y = depot.nix.yants;
  ssh-pubkey = y.restrict "SSH pubkey" (lib.hasPrefix "ssh-") y.string;
  secret-bunch = y.struct { publicKeys = y.list ssh-pubkey; };
in

# TODO: fix the return type, and yantsify readTree.drvTargets
y.defun [ y.path (y.attrs secret-bunch) (y.attrs y.any) ]
  (path: secrets:
    depot.nix.readTree.drvTargets
      # Import each secret into the Nix store
      (builtins.mapAttrs (name: _: "${path}/${name}") secrets))
