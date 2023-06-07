let
  flake = builtins.getFlake "syndicate";
  pkgs = import <nixpkgs> { overlays = [ flake.overlays.default ]; };
in pkgs.nix_actor
