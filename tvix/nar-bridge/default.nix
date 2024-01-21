# Target containing just the proto files.

{ depot, pkgs, lib, ... }:

pkgs.buildGoModule {
  name = "nar-bridge";
  src = depot.third_party.gitignoreSource ./.;

  vendorHash = "sha256-9tEpICef6xCuRQwyXCg15KugcvknnP53WHotohcbeis=";
}
