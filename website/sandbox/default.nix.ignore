{ pkgs, briefcase, ... }:

pkgs.stdenv.mkDerivation {
  name = "covid-uk";
  buildInputs = [];
  src = builtins.path { path = ./.; name = "sandbox"; };
  buildPhase = ''
    mkdir -p $out
    cp $src/index.html $out
    cp -r ${briefcase.website.sandbox.covid-uk} $out/covid-uk
  '';
  dontInstall = true;
}
