# Target containing just the proto files.

{ depot, pkgs, lib, ... }:

pkgs.buildGoModule {
  name = "nar-bridge";
  src = depot.third_party.gitignoreSource ./.;

  vendorHash = "sha256-2WrXRbaJBg8ldL92KzCn/Lj0xp+E/kIYeHE9qsJEXAI=";
}
