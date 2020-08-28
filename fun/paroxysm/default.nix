{ depot, ... }:

let
  pkgs = depot.third_party;
in
pkgs.naersk.buildPackage {
  name = "paroxysm";
  version = "0.0.2";
  src = ./.;

  buildInputs = with pkgs; [
    openssl
    pkgconfig
    postgresql.lib
    curl
  ];
}
