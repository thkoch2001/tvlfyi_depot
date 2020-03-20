{ pkgs, briefcase, ... }:

pkgs.stdenv.mkDerivation {
  name = "covid-uk";
  buildInputs = [];
  src = ./.;
  buildPhase = ''
    mkdir -p $out
    cp $src/index.html $out
    cp -r ${briefcase.website.sandbox.covid-uk} $out/covid-uk
  '';
  dontInstall = true;
}
