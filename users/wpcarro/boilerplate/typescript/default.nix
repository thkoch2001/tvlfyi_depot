{ pkgs, ... }:

pkgs.stdenv.mkDerivation {
  name = "typescript";
  srcs = builtins.path { path = ./.; name = "typescript"; };
  buildInputs = with pkgs; [
    nodejs
    # Exposes lscpu for parcel.js
    util-linux
  ];
  # parcel.js needs number of CPUs
  PARCEL_WORKERS = "1";
  buildPhase = ''
    export HOME="."
    npx parcel build src/index.html --public-url ./
  '';
  installPhase = ''
    mv dist $out
  '';

  # TODO(wpcarro): This doesn't build at all.
  meta.ci.skip = true;
}
