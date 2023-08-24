let
  flake = builtins.getFlake "/home/emery/src/syndicate-flake";
  pkgs = import <nixpkgs> { overlays = [ flake.overlays.default ]; };
in pkgs.nim2Packages.nix_actor
