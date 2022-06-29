{ depot, pkgs, lib, ... }:

let
  lorri-wait-for-eval = pkgs.writers.writeHaskell "lorri-wait-for-eval"
    {
      libraries = [
        pkgs.haskellPackages.async
        pkgs.haskellPackages.aeson-better-errors
        pkgs.haskellPackages.conduit-extra
        pkgs.haskellPackages.error
        pkgs.haskellPackages.PyF
        pkgs.haskellPackages.unliftio
      ];
      ghcArgs = [ "-threaded" ];

    } ./LorriWaitForEval.hs ;

in lorri-wait-for-eval
