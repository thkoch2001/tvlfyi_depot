# Target containing just the proto files.

{ depot, pkgs, lib, ... }:

pkgs.buildGoModule {
  name = "nar-bridge";
  src = depot.third_party.gitignoreSource ./.;

  vendorHash = "sha256-ngaWXS9nYIIMexsnt8WRjY5bBmBCNGGdNsedytyZr5M=";
}
