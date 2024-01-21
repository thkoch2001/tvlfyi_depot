# Target containing just the proto files.

{ depot, pkgs, lib, ... }:

pkgs.buildGoModule {
  name = "nar-bridge";
  src = depot.third_party.gitignoreSource ./.;

  vendorHash = "sha256-9VILiA4D/G/JxhT0qufTytZ0oLPiemeOV9ZHaDh3qpQ=";
}
