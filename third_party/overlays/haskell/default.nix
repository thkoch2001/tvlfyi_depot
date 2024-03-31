# Defines an overlay for overriding Haskell packages, for example to
# avoid breakage currently present in nixpkgs or to modify package
# versions.

{ lib, ... }:

self: super: # overlay parameters for the nixpkgs overlay

let
  haskellLib = self.haskell.lib.compose;
in
{
  haskellPackages = super.haskellPackages.override {
    overrides = hsSelf: hsSuper: {

      ihp-hsx = lib.pipe hsSuper.ihp-hsx [
        (haskellLib.overrideSrc {
          version = "unstable-2023-03-28";
          src = "${self.fetchFromGitHub {
            owner = "digitallyinduced";
            repo = "ihp";
            rev = "ab4ecd05f4e7b6b3c4b74b82d39fc6c5cc48766b";
            sha256 = "1fj5q9lygnmvqqv2fwqdj12sv63gkdfv5ha6fi190sv07dp9n9an";
          }}/ihp-hsx";
        })
        haskellLib.doJailbreak
      ];

      pa-prelude = hsSelf.callPackage ./extra-pkgs/pa-prelude.nix { };
      pa-error-tree = hsSelf.callPackage ./extra-pkgs/pa-error-tree-0.1.0.0.nix { };
      pa-field-parser = hsSelf.callPackage ./extra-pkgs/pa-field-parser.nix { };
      pa-label = hsSelf.callPackage ./extra-pkgs/pa-label.nix { };
      pa-pretty = hsSelf.callPackage ./extra-pkgs/pa-pretty-0.1.1.0.nix { };
      pa-json = hsSelf.callPackage ./extra-pkgs/pa-json.nix { };
      pa-run-command = hsSelf.callPackage ./extra-pkgs/pa-run-command-0.1.0.0.nix { };
    };
  };

  haskell = lib.recursiveUpdate super.haskell {
    packages.ghc8107 = super.haskell.packages.ghc8107.override {
      overrides = hsSelf: hsSuper: {
        # TODO(sterni): TODO(grfn): patch xanthous to work with random-fu 0.3.*,
        # so we can use GHC 9.0.2 and benefit from upstream binary cache.
        random-fu = hsSelf.callPackage ./extra-pkgs/random-fu-0.2.nix { };
        rvar = hsSelf.callPackage ./extra-pkgs/rvar-0.2.nix { };

        # TODO(grfn): port to brick 1.4 (EventM gains an additional type argument in 1.0)
        brick = hsSelf.callPackage ./extra-pkgs/brick-0.73.nix { };
      };
    };
  };
}
