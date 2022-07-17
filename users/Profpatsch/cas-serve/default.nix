{ depot, pkgs, lib, ... }:

let
  cas-serve = pkgs.writers.writeHaskell "cas-serve"
    {
      libraries = [
        pkgs.haskellPackages.wai
        pkgs.haskellPackages.warp
        pkgs.haskellPackages.sqlite-simple
        depot.users.Profpatsch.my-prelude
        (pkgs.haskell.lib.dontCheck
          (pkgs.haskell.lib.overrideSrc pkgs.haskellPackages.superrecord {
            src = pkgs.fetchFromGitHub {
              owner = "Profpatsch";
              repo = "superrecord";
              rev = "c00e933f582e3fb8d209f6cece91d464faf09082";
              sha256 = "sha256-UQ2wCoBpUEPcRsI7wNOFGH+vceKF4dcbbGHFVVTkOWw=";
            };
          }))

      ];
      ghcArgs = [ "-threaded" ];

    } ./CasServe.hs;

in
cas-serve
