{ pkgs, ... }:

pkgs.stdenv.mkDerivation {
  name = "learn.wpcarro.dev";
  src = ./static;
  buildPhase = ''
    cp -R $src $out
  '';
  dontInstall = true;
}
