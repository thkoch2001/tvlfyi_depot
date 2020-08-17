{ pkgs, briefcase, ... }:

pkgs.stdenv.mkDerivation {
  name = "wpcarro.dev";
  src = builtins.path { path = ./.; name = "website"; };
  installPhase = ''
    mkdir -p $out
    cp $src/index.html $out

    mkdir -p $out/habits
    cp ${briefcase.website.habits} $out/habits/index.html
  '';
}
