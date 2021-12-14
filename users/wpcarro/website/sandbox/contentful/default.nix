{ pkgs, ... }:

pkgs.stdenv.mkDerivation {
  name = "ideal-website";
  src = builtins.path { path = ./.; name = "contentful"; };
  buildInputs = with pkgs; [
    nodejs
    # Exposes lscpu for parcel.js
    utillinux
  ];
  # parcel.js needs number of CPUs
  PARCEL_WORKERS = "1";
  buildPhase = ''
    export HOME="."
    npx parcel build index.html
  '';

  installPhase = ''
    mv dist $out
  '';

  # TODO(wpcarro): This doesn't build at all.
  meta.ci = false;
}
