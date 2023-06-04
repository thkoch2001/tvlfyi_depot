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
      pkgs.haskellPackages.sqlite-simple
      pkgs.haskellPackages.xml-conduit
      depot.users.Profpatsch.arglib.netencode.haskell
      depot.users.Profpatsch.netencode.netencode-hs

    ];

    isExecutable = true;
    isLibrary = false;
    license = lib.licenses.mit;
  };

in
jbovlaste-sqlite
