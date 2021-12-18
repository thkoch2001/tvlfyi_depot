# Defines an overlay for overriding Haskell packages, for example to
# avoid breakage currently present in nixpkgs or to modify package
# versions.

{ lib, ... }:

self: super: # overlay parameters for the nixpkgs overlay

let
  overrides = hsSelf: hsSuper: with super.haskell.lib; {
    generic-arbitrary = appendPatch hsSuper.generic-arbitrary
      [ ./patches/generic-arbitrary-export-garbitrary.patch ];

    nix-diff = overrideSrc hsSuper.nix-diff {
      src = builtins.fetchTarball {
        # https://github.com/Gabriel439/nix-diff/pull/50
        url = "https://github.com/Profpatsch/nix-diff/archive/6234936b26cd144d321039a44cf1791dcfaee026.tar.gz";
        sha256 = "1i3ahh8jmiayf29i1xrl2n1dkldgdmrllmccxi4yx6hbcd0r2b1l";
      };
    };
  };
in
{
  haskellPackages = super.haskellPackages.override {
    inherit overrides;
  };
}
