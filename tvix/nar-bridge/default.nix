# Target containing just the proto files.

{ depot, pkgs, lib, ... }:

pkgs.buildGoModule {
  name = "nar-bridge";
  src = depot.third_party.gitignoreSource ./.;

  vendorHash = "sha256-gieFaLB3vgoSqCARCe8PZeJE8H2YQ48Fj2h2DBzUXu8=";
}
