{ depot, pkgs, lib, ... }:

let
  cas-serve = pkgs.writers.writeHaskell "mailbox-org"
    {
      libraries = [
        depot.users.Profpatsch.my-prelude
        pkgs.haskellPackages.aeson
        pkgs.haskellPackages.http-conduit

      ];
      ghcArgs = [ "-threaded" ];
    } ./MailboxOrg.hs;

in
cas-serve
