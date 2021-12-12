# Expose secrets as part of the tree, making it possible to validate
# their paths at eval time.
#
# Note that encrypted secrets end up in the Nix store, but this is
# fine since they're publicly available anyways.
{ depot, pkgs, ... }:

let
  inherit (builtins) attrNames listToAttrs;

  # Import agenix configuration file, this itself is not a readTree
  # target but defines all valid secrets.
  secrets = import ./secrets.nix;

  # Import a secret to the Nix store
  declareSecret = name: pkgs.runCommandNoCC name {} ''
    cp ${./. + "/${name}"} $out
  '';
in depot.nix.readTree.drvTargets (listToAttrs (
  map (name: { inherit name; value = declareSecret name; }) (attrNames secrets)
))
