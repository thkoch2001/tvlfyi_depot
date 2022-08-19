{ depot, pkgs, lib, ... }:

pkgs.haskellPackages.mkDerivation {
  pname = "my-prelude";
  version = "0.0.1-unreleased";

  src = depot.users.Profpatsch.exactSource ./. [
    ./my-prelude.cabal
    ./MyPrelude.hs
  ];

  isLibrary = true;

  libraryHaskellDepends = [
    pkgs.haskellPackages.PyF
    pkgs.haskellPackages.errors
    pkgs.haskellPackages.profunctors
    pkgs.haskellPackages.semigroupoids
    pkgs.haskellPackages.these
    pkgs.haskellPackages.validation-selective
    pkgs.haskellPackages.error

  ];

  license = lib.licenses.mit;

}
