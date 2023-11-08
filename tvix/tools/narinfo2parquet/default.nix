{ pkgs, ... }:

(pkgs.callPackage ./Cargo.nix { }).rootCrate.build
