{ depot, pkgs, lib, ... }:

let
  inherit (depot.users.flokli.nixos)
    archeology;

  systemFor = sys: (depot.ops.nixos.nixosFor sys).system;

in
{
  archeologySystem = (depot.ops.nixos.nixosFor ({ ... }: {
    imports = [
      ./archeology/configuration.nix
    ];
  })).config.system.build.toplevel;
}
