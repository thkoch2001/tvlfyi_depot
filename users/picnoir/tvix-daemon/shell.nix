{ pkgs, ... }:
pkgs.mkShell {
  name = "tvix-daemon";
  packages = [
    pkgs.cargo
    pkgs.cargo-machete
    pkgs.cargo-expand
    pkgs.clippy
    pkgs.evans
    pkgs.fuse
    pkgs.go
    pkgs.grpcurl
    pkgs.hyperfine
    pkgs.nix_2_3 # b/313
    pkgs.pkg-config
    pkgs.rust-analyzer
    pkgs.rustc
    pkgs.rustfmt
    pkgs.protobuf
  ];
}
