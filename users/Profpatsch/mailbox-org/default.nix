{ depot, pkgs, lib, ... }:

let

  cas-serve =
    lib.pipe ./MailboxOrg.hs [
      (depot.users.Profpatsch.writers.writeHaskellInteractive "mailbox-org"
        {
          libraries = [
            depot.users.Profpatsch.my-prelude
            depot.users.Profpatsch.execline.exec-helpers-hs
            depot.users.Profpatsch.arglib.netencode.haskell
            pkgs.haskellPackages.aeson
            pkgs.haskellPackages.http-conduit
            pkgs.haskellPackages.aeson-better-errors

          ];
          ghcArgs = [ "-threaded" ];
        })
      (depot.users.Profpatsch.arglib.netencode.with-args {
        BINS = depot.nix.getBins pkgs.dovecot_pigeonhole [ "sieve-test" ];
      })
    ];


in
cas-serve
