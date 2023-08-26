{ pkgs, lib, ... }:

pkgs.rustPlatform.buildRustPackage {
  name = "gio-list-apps";
  src = lib.cleanSource ./.;
  cargoLock.lockFile = ./Cargo.lock;
  nativeBuildInputs = [ pkgs.pkg-config ];
  buildInputs = [ pkgs.gtk3 ];
}
