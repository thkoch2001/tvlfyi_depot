{ pkgs, ... }:

pkgs.naersk.buildPackage {
  src = ./.;
  buildInputs = [ pkgs.openssl ];
  nativeBuildInputs = [ pkgs.pkgconfig ];
}
