{ pkgs, lib, ... }:

pkgs.stdenv.mkDerivation {
  pname = "tvix-docs";
  version = "0.1";

  outputs = [ "out" ];

  src = lib.cleanSource ./.;

  nativeBuildInputs = [
    pkgs.mdbook
    pkgs.mdbook-plantuml
    pkgs.plantuml
  ];

  buildCommand = ''
    cd $src
    mdbook build -d $out
  '';
}
