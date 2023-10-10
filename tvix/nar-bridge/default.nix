# Target containing just the proto files.

{ depot, pkgs, lib, ... }:

pkgs.buildGoModule {
  name = "nar-bridge";
  src = depot.third_party.gitignoreSource ./.;

  vendorHash = "sha256-Lb0MOziF86JrnrA9SibHYQAaS7H054Nuf3l8tm/9Sf8=";
}
