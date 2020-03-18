{ pkgs, ... }:

pkgs.stdenv.mkDerivation {
  name = "covid-uk";
  buildInputs = [];
  src = ./.;
  buildPhase = ''
    mkdir -p $out
    mkdir -p $out/node_modules/chart.js/dist
    cp $src/node_modules/chart.js/dist/Chart.bundle.min.js $out/node_modules/chart.js/dist
    cp $src/src/* $out
  '';
  dontInstall = true;
}
