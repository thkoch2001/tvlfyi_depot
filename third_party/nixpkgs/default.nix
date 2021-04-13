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

  # Tracking nixos-unstable as of 2021-04-17.
  unstableHashes = {
    commit = "e5cc06a1e806070693add4f231060a62b962fc44";
    sha256 = "04543i332fx9m7jf6167ac825s4qb8is0d0x0pz39il979mlc87v";
  };

  # Tracking nixos-20.09 as of 2021-04-17.
  stableHashes = {
    commit = "c7e905b6a971dad2c26a31960053176c770a812a";
    sha256 = "1h0npga8nlbmv2fnirmv8v5xs8xd3k7hn803lxjn09hyqdcxx23n";
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
    inherit (stableNixpkgs)
      awscli # TODO(grfn): Move back to unstable once it is fixed
      ;
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
