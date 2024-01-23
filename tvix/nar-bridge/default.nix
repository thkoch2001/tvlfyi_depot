# Target containing just the proto files.

{ depot, pkgs, lib, ... }:

pkgs.buildGoModule {
  name = "nar-bridge";
  src = depot.third_party.gitignoreSource ./.;

  vendorHash = "sha256-nFwkq6GTQ1maw6rDeR69M6r+gvkd/7U0vuBfOFd5iFw=";
}
