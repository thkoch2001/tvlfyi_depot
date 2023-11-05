# Target containing just the proto files.

{ depot, pkgs, lib, ... }:

pkgs.buildGoModule {
  name = "nar-bridge";
  src = depot.third_party.gitignoreSource ./.;

  vendorHash = "sha256-AFi+3grLMANb5JmcQx4Y4s2fc/zlFZCLziglHgqR06o=";
}
