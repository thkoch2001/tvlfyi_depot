{ pkgs, ... }:

pkgs.stdenv.mkDerivation {
  name = "goals";
  src = ./.;
  installPhase = ''
    mkdir -p $out
    cp $srcs/index.{html,jsx} $out
  '';
}
