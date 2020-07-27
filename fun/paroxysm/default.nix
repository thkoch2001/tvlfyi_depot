{ depot, ... }:

let
  pkgs = depot.third_party;
in
pkgs.naersk.buildPackage {
  name = "tvldb";
  version = "0.0.1";
  src = ./.;
  buildInputs = [pkgs.openssl pkgs.pkgconfig pkgs.postgresql.lib];
}
