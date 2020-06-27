{ lib, pkgs, depot, ... }:

let

  getBins = import ./getBins.nix { inherit lib; };

  tests = import ./tests.nix {
    inherit getBins;
    inherit (pkgs) writeScriptBin;
    inherit (depot.nix.runTestsuite) assertEq it runTestsuite;
  };

in {
  __functor = _: getBins;
  inherit tests;
}
