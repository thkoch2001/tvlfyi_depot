{ depot, pkgs, lib, ... }:

let
  #   bins = depot.nix.getBins pkgs.sqlite ["sqlite3"];

  whatcd-resolver = pkgs.haskellPackages.mkDerivation {
    pname = "whatcd-resolver";
    version = "0.1.0";

    src = depot.users.Profpatsch.exactSource ./. [
      ./whatcd-resolver.cabal
      ./src/WhatcdResolver.hs
    ];

    libraryHaskellDepends = [
      depot.users.Profpatsch.my-prelude
      depot.users.Profpatsch.my-webstuff
      pkgs.haskellPackages.pa-prelude
      pkgs.haskellPackages.pa-label
      pkgs.haskellPackages.pa-json
      pkgs.haskellPackages.pa-error-tree
      pkgs.haskellPackages.pa-field-parser
      pkgs.haskellPackages.pa-pretty
      pkgs.haskellPackages.pa-run-command
      pkgs.haskellPackages.aeson-better-errors
      pkgs.haskellPackages.blaze-html
      pkgs.haskellPackages.dlist
      pkgs.haskellPackages.http-conduit
      pkgs.haskellPackages.http-types
      pkgs.haskellPackages.ihp-hsx
      pkgs.haskellPackages.monad-logger
      pkgs.haskellPackages.resource-pool
      pkgs.haskellPackages.postgresql-simple
      pkgs.haskellPackages.selective
      pkgs.haskellPackages.tmp-postgres
      pkgs.haskellPackages.unliftio
      pkgs.haskellPackages.wai-extra
      pkgs.haskellPackages.warp
    ];

    isExecutable = true;
    isLibrary = false;
    license = lib.licenses.mit;
  };

in
whatcd-resolver
