# This file imports the pinned nixpkgs sets and applies relevant
# modifications, such as our overlays.
#
# Note that the attribute exposed by this (third_party.nixpkgs) is
# "special" in that the fixpoint used as readTree's config parameter
# in //default.nix passes this attribute as the `pkgs` argument to all
# readTree derivations.

{ depot, ... }:

let
  stableNixpkgs = import depot.third_party.stableNixpkgsSrc {};

  # Overlay for packages that should come from the stable channel
  # instead (e.g. because something is broken in unstable).
  stableOverlay = self: super: {
    inherit (stableNixpkgs)
      awscli # TODO(grfn): Move back to unstable once it is fixed
      ;
  };
in import depot.third_party.nixpkgsSrc {
  config.allowUnfree = true;
  config.allowBroken = true;
  overlays = [
    stableOverlay
    depot.third_party.tvlOverlay
  ];
}
