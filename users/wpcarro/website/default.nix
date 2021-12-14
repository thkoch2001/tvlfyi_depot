{ pkgs, depot, ... }:

pkgs.stdenv.mkDerivation {
  name = "wpcarro.dev";
  src = builtins.path { path = ./.; name = "website"; };
  installPhase = ''
    mkdir -p $out
    cp $src/index.html $out

    mkdir -p $out/habits
    cp -r ${depot.users.wpcarro.website.habit-screens} $out/habits/index.html
  '';
}
