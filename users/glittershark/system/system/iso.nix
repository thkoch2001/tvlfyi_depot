{ depot, lib, pkgs, ... }:

let
  inherit (depot.third_party) partialNixos;

  configuration = { ... }: {
    imports = [
      "${pkgs.path}/nixos/modules/installer/cd-dvd/installation-cd-minimal-new-kernel.nix"
      "${pkgs.path}/nixos/modules/installer/cd-dvd/channel.nix"
    ];

    networking.networkmanager.enable = true;
    networking.useDHCP = false;
    networking.firewall.enable = false;
    networking.wireless.enable = lib.mkForce false;
  };
in (partialNixos {
  inherit configuration;
}).config.system.build.isoImage
