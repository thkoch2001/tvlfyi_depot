{ depot, pkgs, lib, ... }:

pkgs.haskellPackages.mkDerivation {
  pname = "my-webstuff";
  version = "0.0.1-unreleased";

  src = depot.users.Profpatsch.exactSource ./. [
    ./my-webstuff.cabal
    ./src/Multipart2.hs
  ];

  isLibrary = true;

  libraryHaskellDepends = [
    depot.users.Profpatsch.my-prelude
    pkgs.haskellPackages.dlist
    pkgs.haskellPackages.monad-logger
    pkgs.haskellPackages.pa-error-tree
    pkgs.haskellPackages.pa-field-parser
    pkgs.haskellPackages.pa-prelude
    pkgs.haskellPackages.selective
    pkgs.haskellPackages.wai-extra
  ];

  license = lib.licenses.mit;

}
