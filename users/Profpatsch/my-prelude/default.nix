{ depot, pkgs, lib, ... }:

pkgs.haskellPackages.mkDerivation {
  pname = "my-prelude";
  version = "0.0.1-unreleased";

  src = depot.users.Profpatsch.exactSource ./. [
    ./my-prelude.cabal
    ./MyPrelude.hs
    ./Label.hs
    ./Pretty.hs
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

    pkgs.haskellPackages.hscolour
    pkgs.haskellPackages.nicify-lib
    pkgs.haskellPackages.ansi-terminal
  ];

  license = lib.licenses.mit;

}
