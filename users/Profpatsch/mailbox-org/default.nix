{ depot, pkgs, lib, ... }:

let

  cas-serve = depot.users.Profpatsch.writers.writeHaskellInteractive "mailbox-org"
    {
      libraries = [
        depot.users.Profpatsch.my-prelude
        depot.users.Profpatsch.execline.exec-helpers-hs
        pkgs.haskellPackages.aeson
        pkgs.haskellPackages.http-conduit
        pkgs.haskellPackages.aeson-better-errors

      ];
      ghcArgs = [ "-threaded" ];
    } ./MailboxOrg.hs;

in
cas-serve
