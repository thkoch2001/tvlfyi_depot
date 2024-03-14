{
  depot,
  pkgs,
  lib,
  ...
}:

let
  #   bins = depot.nix.getBins pkgs.sqlite ["sqlite3"];

  my-xmonad = pkgs.haskellPackages.mkDerivation {
    pname = "my-xmonad";
    version = "0.1.0";

    src = depot.users.Profpatsch.exactSource ./. [
      ./my-xmonad.cabal
      ./Xmonad.hs
    ];

    libraryHaskellDepends = [ pkgs.haskellPackages.xmonad-contrib ];

    isExecutable = true;
    isLibrary = false;
    license = lib.licenses.mit;
  };
in
my-xmonad
