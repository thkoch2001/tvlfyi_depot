{ pkgs, briefcase, ... }:

pkgs.stdenv.mkDerivation {
  name = "wpcarro.dev";
  src = ./.;
  installPhase = ''
    mkdir -p $out
    cp $src/index.html $out

    mkdir -p $out/goals
    cp -r ${briefcase.website.goals}/* $out/goals
  '';
}
