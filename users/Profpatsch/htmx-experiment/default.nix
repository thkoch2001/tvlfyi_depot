{
  depot,
  pkgs,
  lib,
  ...
}:

let
  htmx-experiment = pkgs.haskellPackages.mkDerivation {
    pname = "htmx-experiment";
    version = "0.1.0";

    src = depot.users.Profpatsch.exactSource ./. [
      ./htmx-experiment.cabal
      ./Main.hs
      ./src/HtmxExperiment.hs
      ./src/ServerErrors.hs
      ./src/ValidationParseT.hs
    ];

    libraryHaskellDepends = [
      depot.users.Profpatsch.my-webstuff
      pkgs.haskellPackages.pa-label
      pkgs.haskellPackages.pa-error-tree
      pkgs.haskellPackages.blaze-html
      pkgs.haskellPackages.blaze-markup
      pkgs.haskellPackages.bytestring
      pkgs.haskellPackages.dlist
      pkgs.haskellPackages.http-types
      pkgs.haskellPackages.ihp-hsx
      pkgs.haskellPackages.monad-logger
      pkgs.haskellPackages.pa-error-tree
      pkgs.haskellPackages.pa-field-parser
      pkgs.haskellPackages.pa-label
      pkgs.haskellPackages.pa-prelude
      pkgs.haskellPackages.selective
      pkgs.haskellPackages.text
      pkgs.haskellPackages.unliftio
      pkgs.haskellPackages.wai
      pkgs.haskellPackages.warp
    ];

    isLibrary = false;
    isExecutable = true;
    license = lib.licenses.mit;
  };
in
htmx-experiment
