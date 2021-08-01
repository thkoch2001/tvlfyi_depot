# Apply patches to a directory or an archive (produced by a path or a derivation)
# by reusing `stdenv.mkDerivation`'s `patchPhase`. This is intended for use with
# simple builders like `//nix/buildLisp` which have no support for patches.
{ pkgs, ... }:

{ # name of the resulting derivation
  name
  # input directory or archive
, src
  # patch files to apply
, patches
  # pre / post phase hook you can override
, preUnpack ? ""
, postUnpack ? ""
, prePatch ? ""
, postPatch ? ""
, preInstall ? ""
, postInstall ? ""
}:

pkgs.stdenv.mkDerivation {
  inherit
    name
    src
    patches
    preUnpack
    postUnpack
    prePatch
    postPatch
    preInstall
    postInstall
    ;

  phases = [
    "unpackPhase"
    "patchPhase"
    "installPhase"
  ];

  installPhase = ''
    runHook preInstall

    cp --reflink=auto -a . "$out"

    runHook postInstall
  '';
}
