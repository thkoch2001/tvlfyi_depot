# Target containing just the proto files.

{ depot, pkgs, lib, ... }:

pkgs.buildGoModule {
  name = "nar-bridge-go";
  src = depot.third_party.gitignoreSource ./.;

  vendorHash = "sha256-7jugbC5sEGhppjiZgnoLP5A6kQSaHK9vE6cXVZBG22s=";
}
