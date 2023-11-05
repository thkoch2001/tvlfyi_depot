# Target containing just the proto files.

{ depot, pkgs, lib, ... }:

pkgs.buildGoModule {
  name = "nar-bridge";
  src = depot.third_party.gitignoreSource ./.;

  vendorHash = "sha256-SFjMlEremD/gCShFDwMCJZyC3oZCTQg06g1L7CG1Ndw=";
}
