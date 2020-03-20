{ pkgs, ... }:

pkgs.stdenv.mkDerivation {
  name = "wpcarro.dev";
  src = ./.;
  buildPhase = ''
    mkdir -p $out
    cp $src/index.html $out
  '';
  dontInstall = true;
}
