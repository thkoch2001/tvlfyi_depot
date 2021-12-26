# Expose secrets as part of the tree, making it possible to validate
# their paths at eval time.
#
# Note that encrypted secrets end up in the Nix store, but this is
# fine since they're publicly available anyways.
#
# Type: mkSecrets :: <path> -> [<secrets>] -> <attrset including meta.targets>
{ depot, pkgs, ... }:

let
  inherit (builtins) foldl' mapAttrs;

  # Type: flip-pipe :: [<functions>] -> a -> <return type of last function>
  # equivalent to `with pkgs.lib; flip pipe`
  flip-pipe = foldl' (x: f: f x);
in

path: flip-pipe [
  # Import each secret into the Nix store
  (mapAttrs (name: _: "${path}/${name}"))
  depot.nix.readTree.drvTargets
] /* secrets */
