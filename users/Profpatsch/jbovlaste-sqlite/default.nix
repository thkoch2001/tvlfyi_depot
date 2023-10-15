{ depot, pkgs, lib, ... }:

let
  #   bins = depot.nix.getBins pkgs.sqlite ["sqlite3"];

  jbovlaste-sqlite = pkgs.haskellPackages.mkDerivation {
    pname = "jbovlaste-sqlite";
    version = "0.1.0";

    src = depot.users.Profpatsch.exactSource ./. [
      ./jbovlaste-sqlite.cabal
      ./JbovlasteSqlite.hs
    ];

    libraryHaskellDepends = [
      pkgs.haskellPackages.pa-prelude
      pkgs.haskellPackages.pa-label
      pkgs.haskellPackages.pa-error-tree
      pkgs.haskellPackages.pa-field-parser
      depot.users.Profpatsch.my-prelude
      pkgs.haskellPackages.foldl
      pkgs.haskellPackages.sqlite-simple
      pkgs.haskellPackages.xml-conduit

    ];

    isExecutable = true;
    isLibrary = false;
    license = lib.licenses.mit;
  };

in
jbovlaste-sqlite
