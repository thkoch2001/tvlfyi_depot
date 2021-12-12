{ depot, pkgs, ... }:

depot.third_party.naersk.buildPackage {
  src = ./.;

  buildInputs = with pkgs; [ pkgconfig openssl systemd.dev ];
}
