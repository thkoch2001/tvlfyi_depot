{ depot, pkgs, lib, ... }:
let
  runExecline = import ./runExecline.nix {
    inherit (pkgs) stdenv;
    inherit (depot.nix) escapeExecline getBins;
    inherit pkgs lib;
  };

  tests = import ./tests.nix {
    inherit runExecline;
    inherit (depot.nix) getBins;
    inherit (pkgs) stdenv coreutils;
    inherit pkgs;
  };

in {
  __functor = _: runExecline;
  inherit tests;
}
