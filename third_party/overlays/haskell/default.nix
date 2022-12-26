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
      # backport of https://github.com/dhall-lang/dhall-haskell/pull/2426
      # to hnix 0.14
      rev = "466bfe757cc12625279a4722bdb8c939e0f0dccd"; # "fix-dhall-to-nix-key-encoding-hnix-0.14";
      sha256 = "sha256-9IbDFcMQbxifbzuJjEtnm945jrfvR/FwnLzo0JUhO8I=";
    }}/${subdir}";
  };


in
{
  haskellPackages = super.haskellPackages.override {
    overrides = hsSelf: hsSuper: {
      # TODO: this is to fix a bug in dhall-nix
      dhall = dhall-source "dhall" hsSuper.dhall;
      dhall-nix = dhall-source "dhall-nix" hsSuper.dhall-nix;
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
