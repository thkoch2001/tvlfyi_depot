# Target containing just the proto files.

{ depot, pkgs, lib, ... }:

pkgs.buildGoModule {
  name = "nar-bridge";
  src = depot.third_party.gitignoreSource ./.;

  vendorHash = "sha256-BO7PvapQyyGJdOJlsd93uuGkIfTK94WdBeNFE+/XwWI=";
}
