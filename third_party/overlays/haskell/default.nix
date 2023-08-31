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

  hgeometry-source = subdir: pkg:
    haskellLib.overrideSrc
      {
        src = "${super.fetchFromGitHub {
          owner = "noinia";
          repo = "hgeometry";
          # https://github.com/noinia/hgeometry/pull/220
          rev = "f8cdbc02c6878d4883ce95ed229d9cb09ffe296f";
          sha256 = "sha256-7XsmUetYu3iCS7eNImHPU0Po3YKE/sGxBHEPT5N6nwU=";
        }}/${subdir}";
      }
      (haskellLib.overrideCabal { patches = [ ]; } pkg);



in
{
  haskellPackages = super.haskellPackages.override {
    overrides = hsSelf: hsSuper: {

      ihp-hsx = lib.pipe hsSuper.ihp-hsx [
        (haskellLib.overrideSrc {
          version = "1.1.0";
          src = "${self.fetchFromGitHub {
            owner = "digitallyinduced";
            repo = "ihp";
            rev = "b5d47963c998ccd779aa5c3d46484338fd621f0d";
            sha256 = "sha256-M22W8VX4sRaeU2yVraR0S2t2VOwWGmoteD/M8TahdoE=";
          }}/ihp-hsx";
        })
        haskellLib.doJailbreak
      ];

      vector-circular = haskellLib.doJailbreak hsSuper.vector-circular;

      hgeometry = lib.pipe hsSuper.hgeometry [
        (hgeometry-source "hgeometry")
        (haskellLib.addBuildDepends [ hsSelf.singletons ])
        # some quickcheck tests fail https://github.com/noinia/hgeometry/issues/223
        haskellLib.dontCheck
      ];
      hgeometry-combinatorial = hgeometry-source "hgeometry-combinatorial" hsSuper.hgeometry-combinatorial;
      random-extras = lib.pipe hsSuper.random-extras [
        (haskellLib.overrideSrc {
          src = self.fetchFromGitHub {
            owner = "aristidb";
            repo = "random-extras";
            rev = "b05f7fad403794c225391d2d48e9c8467d2df8e1";
            sha256 = "sha256-Z7eskuv/V5jY5ndpxNhs7WlZZg59Mm80vXfMxF4Dllg=";
          };
        })
      ];
      random-source = null;
      # TODO(grfn): port to brick 1.4 (EventM gains an additional type argument in 1.0)
      brick = lib.pipe { } [
        (_: hsSelf.callPackage ./extra-pkgs/brick-0.73.nix { })
        haskellLib.doJailbreak
      ];

      # TODO: this is to fix a bug in dhall-nix
      dhall = dhall-source "dhall" hsSuper.dhall;
      dhall-nix = dhall-source "dhall-nix" hsSuper.dhall-nix;

      pa-prelude = hsSelf.callPackage ./extra-pkgs/pa-prelude-0.1.0.0.nix { };
      pa-error-tree = hsSelf.callPackage ./extra-pkgs/pa-error-tree-0.1.0.0.nix { };
      pa-field-parser = hsSelf.callPackage ./extra-pkgs/pa-field-parser-0.1.0.1.nix { };
      pa-label = hsSelf.callPackage ./extra-pkgs/pa-label-0.1.0.1.nix { };
      pa-pretty = hsSelf.callPackage ./extra-pkgs/pa-pretty-0.1.1.0.nix { };
      pa-json = hsSelf.callPackage ./extra-pkgs/pa-json-0.2.1.0.nix { };
      pa-run-command = hsSelf.callPackage ./extra-pkgs/pa-run-command-0.1.0.0.nix { };
    };
  };
}
