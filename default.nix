{
  pkgs ? import <nixpkgs> { },
}:

let
  inherit (pkgs) lib;
  buildNimSbom = pkgs.callPackage ./build-nim-sbom.nix { };
in
buildNimSbom (finalAttrs: {
  outputs = [
    "out"
    "cfg"
  ];
  nativeBuildInputs = [ pkgs.pkg-config ];
  buildInputs = [ pkgs.nixVersions.latest ];
  src = if lib.inNixShell then null else lib.cleanSource ./.;
  postInstall = ''
    mkdir $cfg
    export mainProgram="$out/bin/nix-actor"
    substituteAll service.pr.in $cfg/service.pr
  '';
}) ./sbom.json
