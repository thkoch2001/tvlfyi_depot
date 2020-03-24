let
  pkgs = import <nixpkgs> {};
in pkgs.stdenv.mkDerivation {
  name = "ideal-website";
  srcs = ./.;
  buildInputs = with pkgs; [
    nodejs
    # Exposes lscpu for parcel.js
    utillinux
  ];
  # parcel.js needs number of CPUs
  PARCEL_WORKERS = "1";
  buildPhase = ''
    npx parcel build index.html
  '';
  installPhase = ''
    mv dist $out
  '';
}
