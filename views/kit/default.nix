# Externally importable TVL depot stack. This is intended to be called
# with a supplied package set, otherwise the package set currently in
# use by the TVL depot will be used.

{ pkgs ? import ./.nixpkgs { depotOverlays = false; }, ... }:

let
  readTree = import ./readTree {};
  readKit = args: readTree {
    inherit args;
    path = ./.;
  };
in readTree.fix(kit: (readKit {
  # Some targets in tvl-kit expect arguments of the form used in the
  # main TVL depot tree. This attribute set mirrors that layout.
  depot.nix.buildGo = kit.buildGo;

  # Some targets expect a pkgs and lib parameter.
  inherit pkgs;
  inherit (pkgs) lib;
}))
