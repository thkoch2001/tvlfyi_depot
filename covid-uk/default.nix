{ pkgs, ... }:

pkgs.stdenv.mkDerivation {
  name = "covid-uk";
  buildInputs = [];
  src = ./.;
  # TODO(wpcarro): Need to run `yarn install` somehow.
  # TODO(wpcarro): Need to run `npx tailwindcss build styles.css -o output.css`.
  buildPhase = ''
    mkdir -p $out
    mkdir -p $out/node_modules/chart.js/dist
    cp $src/node_modules/chart.js/dist/Chart.bundle.min.js $out/node_modules/chart.js/dist
    cp $src/index.html $src/output.css $out
  '';
  dontInstall = true;
}
