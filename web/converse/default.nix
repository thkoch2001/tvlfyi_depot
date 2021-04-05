{ pkgs, ... }:

pkgs.naersk.buildPackage {
  src = ./.;
  buildInputs = with pkgs; [
    openssl pkgconfig
  ];
}
