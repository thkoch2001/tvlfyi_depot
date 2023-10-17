# Target containing just the proto files.

{ depot, pkgs, lib, ... }:

pkgs.buildGoModule {
  name = "nar-bridge";
  src = depot.third_party.gitignoreSource ./.;

  vendorHash = "sha256-kvovi5HJafg3Um64XJxBYm392tih0P6C/SDyCLhKHIA=";
}
