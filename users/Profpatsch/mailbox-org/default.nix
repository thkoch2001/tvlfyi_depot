{ depot, pkgs, lib, ... }:

let
  mailbox-org = pkgs.haskellPackages.mkDerivation {
    pname = "mailbox-org";
    version = "0.1.0";

    src = depot.users.Profpatsch.exactSource ./. [
      ./mailbox-org.cabal
      ./AesonQQ.hs
      ./MailboxOrg.hs
    ];

    libraryHaskellDepends = [
      depot.users.Profpatsch.my-prelude
      depot.users.Profpatsch.execline.exec-helpers-hs
      depot.users.Profpatsch.arglib.netencode.haskell
      pkgs.haskellPackages.pa-prelude
      pkgs.haskellPackages.aeson
      pkgs.haskellPackages.http-conduit
      pkgs.haskellPackages.aeson-better-errors
    ];

    isLibrary = false;
    isExecutable = true;
    license = lib.licenses.mit;
  };


in
lib.pipe mailbox-org [
  (x: (depot.nix.getBins x [ "mailbox-org" ]).mailbox-org)
  (depot.users.Profpatsch.arglib.netencode.with-args "mailbox-org" {
    BINS = depot.nix.getBins pkgs.dovecot_pigeonhole [ "sieve-test" ];
  })
]
