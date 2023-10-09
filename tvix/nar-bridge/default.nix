# Target containing just the proto files.

{ depot, pkgs, lib, ... }:

pkgs.buildGoModule {
  name = "nar-bridge";
  src = depot.third_party.gitignoreSource ./.;

  vendorHash = "sha256-3Ywc6rrt56U5K5mF5muXPMscD38zS5NIiVPpcYcmIYQ=";
}
