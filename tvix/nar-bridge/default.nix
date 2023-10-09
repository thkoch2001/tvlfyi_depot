# Target containing just the proto files.

{ depot, pkgs, lib, ... }:

pkgs.buildGoModule {
  name = "nar-bridge";
  src = depot.third_party.gitignoreSource ./.;

  vendorHash = "sha256-UxKTfy1NNtZhor8Hj9LZja72vqW7OYDRn8/cUETPzoU=";
}
