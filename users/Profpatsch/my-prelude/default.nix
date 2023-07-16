{ depot, pkgs, lib, ... }:

pkgs.haskellPackages.mkDerivation {
  pname = "my-prelude";
  version = "0.0.1-unreleased";

  src = depot.users.Profpatsch.exactSource ./. [
    ./my-prelude.cabal
    ./src/Aeson.hs
    ./src/MyPrelude.hs
    ./src/Pretty.hs
    ./src/RunCommand.hs
    ./src/Test.hs
    ./src/Tool.hs
    ./src/ValidationParseT.hs
    ./src/Postgres/Decoder.hs
    ./src/Postgres/MonadPostgres.hs
  ];

  isLibrary = true;

  libraryHaskellDepends = [
    pkgs.haskellPackages.pa-prelude
    pkgs.haskellPackages.pa-label
    pkgs.haskellPackages.pa-error-tree
    pkgs.haskellPackages.pa-json
    pkgs.haskellPackages.aeson-better-errors
    pkgs.haskellPackages.ansi-terminal
    pkgs.haskellPackages.error
    pkgs.haskellPackages.hscolour
    pkgs.haskellPackages.hspec
    pkgs.haskellPackages.hspec-expectations-pretty-diff
    pkgs.haskellPackages.monad-logger
    pkgs.haskellPackages.nicify-lib
    pkgs.haskellPackages.postgresql-simple
    pkgs.haskellPackages.profunctors
    pkgs.haskellPackages.PyF
    pkgs.haskellPackages.semigroupoids
    pkgs.haskellPackages.these
    pkgs.haskellPackages.typed-process
    pkgs.haskellPackages.unliftio
    pkgs.haskellPackages.validation-selective
    pkgs.haskellPackages.vector


  ];

  license = lib.licenses.mit;

}
