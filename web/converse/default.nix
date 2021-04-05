{ pkgs, ... }:

pkgs.naersk.buildPackage {
  src = ./.;
  buildInputs = with pkgs; [ openssl postgresql.lib ];
  nativeBuildInputs = [ pkgs.pkgconfig ];
}
