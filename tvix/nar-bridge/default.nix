# Target containing just the proto files.

{ depot, pkgs, lib, ... }:

pkgs.buildGoModule {
  name = "nar-bridge";
  src = depot.third_party.gitignoreSource ./.;

  vendorHash = "sha256-fMZiHb/81/8hQfKXD4b2nwbuSFiNQf1eYxIuAx+93Po=";
}
