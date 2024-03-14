{ lib, pkgs, ... }:

(pkgs.callPackage ./Cargo.nix {
  defaultCrateOverrides = pkgs.defaultCrateOverrides // {

    ring = prev: { links = ''ring_core_${lib.replaceStrings [ "." ] [ "_" ] prev.version}''; };
  };
}).rootCrate.build.override
  { runTests = true; }
