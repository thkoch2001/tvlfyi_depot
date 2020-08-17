{ pkgs, ... }:

pkgs.stdenv.mkDerivation {
  name = "goals-webpage";
  src = builtins.path { path = ./.; name = "goals"; };
  buildInputs = with pkgs; [
    nodejs
    # Exposes lscpu for parcel.js
    utillinux
  ];
  # parcel.js needs number of CPUs
  PARCEL_WORKERS = "1";
  buildPhase = ''
    npx parcel build src/index.html --public-url ./
  '';
  installPhase = ''
    mv dist $out
  '';
}
