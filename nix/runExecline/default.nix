{ depot, pkgs, lib, ... }:
let
  runExecline = import ./runExecline.nix {
    inherit (pkgs) stdenv;
    inherit (depot.nix) escapeExecline getBins;
    inherit lib;
    pkgs = pkgs.buildPackages;
  };

  runExeclineLocal = name: args: execline:
    runExecline name
      (args // {
        derivationArgs = args.derivationArgs or {} // {
          preferLocalBuild = true;
          allowSubstitutes = false;
        };
      })
      execline;

  tests = import ./tests.nix {
    inherit runExecline runExeclineLocal;
    inherit (depot.nix) getBins writeScript;
    inherit (pkgs) stdenv;
    inherit (pkgs.buildPackages) coreutils;
    pkgs = pkgs.buildPackages;
  };

in {
  __functor = _: runExecline;
  local = runExeclineLocal;
  inherit tests;
}
