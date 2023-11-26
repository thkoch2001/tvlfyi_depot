{ depot, pkgs, ... }:

depot.third_party.naersk.buildPackage {
  src = ./.;
  nativeBuildInputs = [ pkgs.pkg-config ];
  buildInputs = [ pkgs.openssl ];
}
