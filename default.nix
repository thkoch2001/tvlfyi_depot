{
  pkgs ? import <nixpkgs> { },
}:

let
  inherit (pkgs) lib;
  buildNimSbom = pkgs.callPackage ./build-nim-sbom.nix { };
in
buildNimSbom (finalAttrs: {
  name = "nix-actor";
  nativeBuildInputs = [ pkgs.pkg-config ];
  buildInputs = [ pkgs.nixVersions.latest ];
  src = if lib.inNixShell then null else lib.cleanSource ./.;
}) ./sbom.json
