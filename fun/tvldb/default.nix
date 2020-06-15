{ depot, pkgs, ... }:

let
  naersk = depot.third_party.naersk;
in
naersk.buildPackage {
  name = "tvldb";
  version = "0.0.1";
  src = ./.;
  buildInputs = [pkgs.openssl pkgs.pkgconfig pkgs.postgresql.lib];
}
