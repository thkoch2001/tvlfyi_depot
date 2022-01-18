# Externally importable TVL depot stack. This is intended to be called
# with a supplied package set, otherwise the package set currently in
# use by the TVL depot will be used.
#
# For now, readTree is not used inside of this configuration.

{ pkgs ? import ./nixpkgs { depotOverlays = false; }, ... }:

pkgs.lib.fix(self: {
  buildGo = import ./buildGo { inherit pkgs; };
  buildkite = import ./buildkite { inherit pkgs; };
  readTree = import ./readTree { };

  besadii = import ./besadii {
    depot.nix.buildGo = self.buildGo;
  };
})
