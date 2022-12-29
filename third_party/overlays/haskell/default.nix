# Defines an overlay for overriding Haskell packages, for example to
# avoid breakage currently present in nixpkgs or to modify package
# versions.

{ lib, ... }:

self: super: # overlay parameters for the nixpkgs overlay

let
  dhall-source = subdir: pkg: super.haskell.lib.overrideSrc pkg {
    src = "${super.fetchFromGitHub {
      owner = "Profpatsch";
      repo = "dhall-haskell";
      # https://github.com/dhall-lang/dhall-haskell/pull/2426
      rev = "82123817316192d39f9a3e68b8ce9c9cff0a48ed";
      sha256 = "sha256-gbHoUKIdLPIttqeV471jsT8OJz6uiI6LpHOwtLbBGHY=";
    }}/${subdir}";
  };


in
{
  haskellPackages = super.haskellPackages.override {
    overrides = hsSelf: hsSuper: {
      # TODO: this is to fix a bug in dhall-nix
      dhall = dhall-source "dhall" hsSuper.dhall;
      dhall-nix = dhall-source "dhall-nix" hsSuper.dhall-nix;

      # GHC 9.2 and newer cabal version support
      graphmod = super.haskell.lib.overrideSrc hsSuper.graphmod {
        src = super.fetchFromGitHub {
          owner = "yav";
          repo = "graphmod";
          rev = "983c38f73d3d6d232c954416fd1ab019f24c9fc5";
          sha256 = "sha256-P+cXEiOZ1b1oFtQEYKZlc/CBYzSV/CAU8YJxlqd1K1w=";
        };
      };
    };
  };

  haskell = lib.recursiveUpdate super.haskell {
    packages.ghc8107 = super.haskell.packages.ghc8107.override {
      overrides = hsSelf: hsSuper: {
        # TODO(sterni): TODO(grfn): patch xanthous to work with random-fu 0.3.*,
        # so we can use GHC 9.0.2 and benefit from upstream binary cache.
        random-fu = hsSelf.callPackage ./extra-pkgs/random-fu-0.2.nix { };
        rvar = hsSelf.callPackage ./extra-pkgs/rvar-0.2.nix { };
      };
    };
  };
}
