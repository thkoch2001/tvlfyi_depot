{ depot, pkgs, lib, ... }:

let

  httzip = pkgs.haskellPackages.mkDerivation {
    pname = "httzip";
    version = "0.1.0";

    src = depot.users.Profpatsch.exactSource ./. [
      ./httzip.cabal
      ./Httzip.hs
    ];

    libraryHaskellDepends = [
      pkgs.haskellPackages.pa-prelude
      pkgs.haskellPackages.warp
      pkgs.haskellPackages.wai
      pkgs.haskellPackages.wai-conduit
      pkgs.haskellPackages.conduit-extra
      pkgs.haskellPackages.conduit
    ];

    isExecutable = true;
    isLibrary = false;
    license = lib.licenses.mit;
  };

  bins = depot.nix.getBins httzip ["httzip"];

in
depot.nix.writeExecline "httzip-wrapped" {} [
      "importas"
      "-ui"
      "PATH"
      "PATH"
      "export"
      "PATH"
      "${pkgs.zip}/bin"
      bins.httzip
]
