{ pkgs, briefcase, ... }:

let
  covid-uk = briefcase.sandbox.covid-uk;
in pkgs.stdenv.mkDerivation {
  name = "covid-uk";
  buildInputs = [];
  src = ./.;
  buildPhase = ''
    mkdir -p $out
    cp $src/index.html $out
    cp -r ${briefcase.sandbox.covid-uk} $out/covid-uk
  '';
  dontInstall = true;
}
