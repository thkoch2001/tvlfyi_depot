{ pkgs, ... }:

pkgs.stdenv.mkDerivation {
  name = "typescript";
  srcs = builtins.path { path = ./.; name = "day-of-week-habits"; };
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
