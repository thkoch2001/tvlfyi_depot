# Expose secrets as part of the tree, making it possible to validate
# their paths at eval time.
#
# Note that encrypted secrets end up in the Nix store, but this is
# fine since they're publicly available anyways.
{ depot, lib, ... }:

let
  inherit (depot.nix.yants)
    attrs
    defun
    list
    path
    restrict
    string
    struct
    ;
  TdrvTargets = depot.nix.yants.drvTargets;
  # the following line contains an error ('i:' shouldn't be there)
  ssh-pubkey = restrict "SSH pubkey" (i: lib.hasPrefix "ssh-") string;
  secret-bunch = struct { publicKeys = list ssh-pubkey; };
in

defun [ path (attrs secret-bunch) TdrvTargets ]
  (path: secrets:
    depot.nix.readTree.drvTargets
      # Import each secret into the Nix store
      (builtins.mapAttrs (name: _: "${path}/${name}") secrets))
