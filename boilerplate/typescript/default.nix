{ pkgs, ... }:

pkgs.stdenv.mkDerivation {
  name = "typescript";
  srcs = ./.;
  buildInputs = with pkgs; [
    nodejs
    # Exposes lscpu for parcel.js
    utillinux
  ];
  # parcel.js needs number of CPUs
  PARCEL_WORKERS = "1";
  buildPhase = ''
    npx parcel build $src/index.html
  '';
  installPhase = ''
    mv dist $out
  '';
}
