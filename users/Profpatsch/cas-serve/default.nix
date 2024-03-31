{ depot, pkgs, lib, ... }:

let
  bins = depot.nix.getBins pkgs.sqlite [ "sqlite3" ];

  cas-serve = pkgs.haskellPackages.mkDerivation {
    pname = "cas-serve";
    version = "0.1.0";

    src = depot.users.Profpatsch.exactSource ./. [
      ./cas-serve.cabal
      ./CasServe.hs
    ];

    libraryHaskellDepends = [
      pkgs.haskellPackages.pa-prelude
      pkgs.haskellPackages.pa-label
      pkgs.haskellPackages.crypton
      pkgs.haskellPackages.wai
      pkgs.haskellPackages.warp
      pkgs.haskellPackages.sqlite-simple
      depot.users.Profpatsch.arglib.netencode.haskell
      depot.users.Profpatsch.netencode.netencode-hs
    ];

    isExecutable = true;
    isLibrary = false;
    license = lib.licenses.mit;
  };

  create-cas-database = depot.nix.writeExecline "create-cas-database" { readNArgs = 1; } [
    bins.sqlite3
    "$1"
    "-init"
    ./schema.sql
  ];
in
cas-serve
