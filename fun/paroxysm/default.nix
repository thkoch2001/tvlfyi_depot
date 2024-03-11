{ pkgs, ... }:

let
  cargoNix = import ./Cargo.nix {
    inherit pkgs;
    nixpkgs = pkgs.path;
  };
in

cargoNix.rootCrate.build
