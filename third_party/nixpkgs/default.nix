# This file imports the pinned nixpkgs sets and applies relevant
# modifications, such as our overlays.
#
# Note that the attribute exposed by this (third_party.nixpkgs) is
# "special" in that the fixpoint used as readTree's config parameter
# in //default.nix passes this attribute as the `pkgs` argument to all
# readTree derivations.

{ depot, externalArgs, ... }:

let
  # This provides the sources of nixpkgs. We track both
  # nixos-unstable, and the current stable channel of the latest NixOS
  # release.

  # Tracking nixos-unstable as of 2021-07-27.
  unstableHashes = {
    commit = "8ecc61c91a596df7d3293603a9c2384190c1b89a";
    sha256 = "0vhajylsmipjkm5v44n2h0pglcmpvk4mkyvxp7qfvkjdxw21dyml";
  };

  # Tracking nixos-21.05 as of 2021-07-27.
  stableHashes = {
    commit = "82151321eeaef290b8345803e0b217a261b7c4e1";
    sha256 = "1n53jn8793midwdwiqk74l4vf0g94wg1bypab4cx1ydlbs20x882";
  };

  # import the nixos-unstable package set, or optionally use the
  # source (e.g. a path) specified by the `nixpkgsBisectPath`
  # argument. This is intended for use-cases where the depot is
  # bisected against nixpkgs to find the root cause of an issue in a
  # channel bump.
  nixpkgsSrc = externalArgs.nixpkgsBisectPath or (fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${unstableHashes.commit}.tar.gz";
    sha256 = unstableHashes.sha256;
  });

  stableNixpkgsSrc = fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${stableHashes.commit}.tar.gz";
    sha256 = stableHashes.sha256;
  };

  # Stable package set is imported, but not exposed, to overlay
  # required packages into the unstable set.
  stableNixpkgs = import stableNixpkgsSrc {};

  # Overlay for packages that should come from the stable channel
  # instead (e.g. because something is broken in unstable).
  stableOverlay = self: super: {
    # nothing picked from stable currently
  };
in import nixpkgsSrc {
  config.allowUnfree = true;
  config.allowBroken = true;
  overlays = [
    stableOverlay
    depot.third_party.overlays.tvl
    depot.third_party.overlays.haskell
    depot.third_party.overlays.emacs
  ];
}
