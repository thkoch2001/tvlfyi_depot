# Defines an overlay for overriding Haskell packages, for example to
# avoid breakage currently present in nixpkgs or to modify package
# versions.
{ lib
, ...
}:
self:
super:
# overlay parameters for the nixpkgs overlay
let
  overrides =
    hsSelf:
    hsSuper:
    with super.haskell.lib;
    {
      generic-arbitrary =
        appendPatch hsSuper.generic-arbitrary [ ./patches/generic-arbitrary-export-garbitrary.patch ];
    };
in
{ haskellPackages = super.haskellPackages.override { inherit overrides; }; }
