{ depot, pkgs, ... }:

depot.third_party.naersk.buildPackage {
  src = ./.;
  buildInputs = with pkgs; [ openssl postgresql.lib ];
  nativeBuildInputs = [ pkgs.pkgconfig ];
}
