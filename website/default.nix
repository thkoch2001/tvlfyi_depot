{ pkgs, briefcase, ... }:

pkgs.stdenv.mkDerivation {
  name = "wpcarro.dev";
  src = ./.;
  installPhase = ''
    mkdir -p $out
    cp $src/index.html $out

    mkdir -p $out/goals
    cp -r ${briefcase.website.goals}/* $out/goals

    mkdir -p $out/habits
    cp ${briefcase.website.habits} $out/habits/index.html
  '';
}
