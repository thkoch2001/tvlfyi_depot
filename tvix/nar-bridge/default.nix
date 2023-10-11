# Target containing just the proto files.

{ depot, pkgs, lib, ... }:

pkgs.buildGoModule {
  name = "nar-bridge";
  src = depot.third_party.gitignoreSource ./.;

  vendorHash = "sha256-drbjsty1hhVAvn/R/AiVIOPQREzHOB+90VpuM5D5COc=";
}
