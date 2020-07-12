{ pkgs ? (import ../../../. {}).third_party
, lib ? pkgs.lib
, ...
}:

(import ./packageSet.nix {}).callPackage (import ./pkg.nix { inherit pkgs; }) {}
