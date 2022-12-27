# Defines an overlay for overriding Haskell packages, for example to
# avoid breakage currently present in nixpkgs or to modify package
# versions.

{ lib, ... }:

self: super: # overlay parameters for the nixpkgs overlay

let
  haskellLib = self.haskell.lib.compose;
  dhall-source = subdir: pkg:
    haskellLib.overrideSrc
      {
        src = "${super.fetchFromGitHub {
          owner = "Profpatsch";
          repo = "dhall-haskell";
          # https://github.com/dhall-lang/dhall-haskell/pull/2426
          rev = "5e3a407d8ac826597d935d8398825a0ca73fc4e9";
          sha256 = "005plj6kgxlkm9npaq07kmsgmiqk50dpwb9li9w1ly4aj1zgfjnd";
        }}/${subdir}";
      }
      (haskellLib.overrideCabal { patches = [ ]; } pkg);


in
{
  haskellPackages = super.haskellPackages.override {
    overrides = hsSelf: hsSuper: {
      # TODO: this is to fix a bug in dhall-nix
      dhall = dhall-source "dhall" hsSuper.dhall;
      dhall-nix = dhall-source "dhall-nix" hsSuper.dhall-nix;

      # TODO(Profpatsch): move cas-serve off superrecord
      # https://github.com/agrafix/superrecord/pull/35
      # https://github.com/agrafix/superrecord/pull/37
      superrecord = haskellLib.overrideSrc
        {
          src = self.fetchFromGitHub {
            owner = "possehl-analytics";
            repo = "superrecord";
            rev = "05c8fdd724af5189a9a8be2f30dfa55a67f8b656";
            sha256 = "0p6a280kils12ycdlp6dd7392940yzzy6xi8pjar975j38fm3x5a";
          };
        }
        hsSuper.superrecord;

      # Use recently-released version that has 9.2 support
      graphmod = assert hsSuper.graphmod != "1.4.5.1";
        hsSelf.callPackage ./extra-pkgs/graphmod-1.4.5.1.nix { };
    };
  };

  haskell = lib.recursiveUpdate super.haskell {
    packages.ghc8107 = super.haskell.packages.ghc8107.override {
      overrides = hsSelf: hsSuper: {
        # TODO(sterni): TODO(grfn): patch xanthous to work with random-fu 0.3.*,
        # so we can use GHC 9.0.2 and benefit from upstream binary cache.
        random-fu = hsSelf.callPackage ./extra-pkgs/random-fu-0.2.nix { };
        rvar = hsSelf.callPackage ./extra-pkgs/rvar-0.2.nix { };

        # TODO(grfn): port to brick 1.4
        # Breaking changes since 0.68:
        # - handleEditorEvent takes a BrickEvent (0.72)
        # - EventM gains an additional type argument (1.0)
        brick = hsSelf.callPackage ./extra-pkgs/brick-0.71.1.nix { };

        # TODO(sterni): upstream into nixpkgs
        binary-orphans =
          haskellLib.addBuildDepends [ hsSelf.OneTuple ] hsSuper.binary-orphans;
      };
    };
  };
}
