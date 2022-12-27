{ depot, pkgs, lib, ... }:

let
  cas-serve = pkgs.writers.writeHaskell "cas-serve"
    {
      libraries = [
        pkgs.haskellPackages.wai
        pkgs.haskellPackages.warp
        pkgs.haskellPackages.sqlite-simple
        pkgs.haskellPackages.superrecord
        depot.users.Profpatsch.my-prelude
      ];
      ghcArgs = [ "-threaded" ];

    } ./CasServe.hs;

in
cas-serve
