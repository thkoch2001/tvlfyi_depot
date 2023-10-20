{ depot, pkgs, lib, ... }:

let
  #   bins = depot.nix.getBins pkgs.sqlite ["sqlite3"];

  openlab-tools = pkgs.haskellPackages.mkDerivation {
    pname = "openlab-tools";
    version = "0.1.0";

    src = depot.users.Profpatsch.exactSource ./. [
      ./openlab-tools.cabal
      ./Main.hs
      ./src/OpenlabTools.hs
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
      pkgs.haskellPackages.deepseq
      pkgs.haskellPackages.case-insensitive
      pkgs.haskellPackages.hs-opentelemetry-sdk
      pkgs.haskellPackages.http-conduit
      pkgs.haskellPackages.http-types
      pkgs.haskellPackages.ihp-hsx
      pkgs.haskellPackages.monad-logger
      pkgs.haskellPackages.selective
      pkgs.haskellPackages.unliftio
      pkgs.haskellPackages.wai-extra
      pkgs.haskellPackages.warp
      pkgs.haskellPackages.tagsoup
      pkgs.haskellPackages.time
    ];

    isExecutable = true;
    isLibrary = false;
    license = lib.licenses.mit;
  };

  bins = depot.nix.getBins openlab-tools [ "openlab-tools" ];

in

depot.nix.writeExecline "openlab-tools-wrapped" { } [
  "importas"
  "-i"
  "PATH"
  "PATH"
  "export"
  "PATH"
  "${pkgs.postgresql}/bin:$${PATH}"
  "export"
  "OPENLAB_TOOLS_TOOLS"
  (pkgs.linkFarm "openlab-tools-tools" [
    {
      name = "pg_format";
      path = "${pkgs.pgformatter}/bin/pg_format";
    }
  ])
  bins.openlab-tools
]

