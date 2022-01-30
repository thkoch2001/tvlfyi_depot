{ depot, pkgs, ... }:

let
  emptyDerivation = import ./emptyDerivation.nix {
    inherit pkgs;
    inherit (pkgs) stdenv;
    inherit (depot.nix) getBins;
  };

  tests = import ./tests.nix {
    inherit emptyDerivation;
    inherit pkgs;
    inherit (depot.nix) writeExecline getBins;
    inherit (depot.nix.runTestsuite) runTestsuite it assertEq;
  };

in
{
  __functor = _: emptyDerivation;
  inherit tests;
}
