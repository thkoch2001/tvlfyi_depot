{ depot, pkgs, ... }:

depot.third_party.naersk.buildPackage {
  src = ./.;

  buildInputs = with pkgs; [
    pkg-config
    openssl
    systemd.dev
  ];
}
