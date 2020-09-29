{ pkgs, ... }:

let
  configuration = { ... }: {
    imports = [
      "${pkgs.nixpkgsSrc}/nixos/modules/installer/cd-dvd/installation-cd-minimal-new-kernel.nix"
      "${pkgs.nixpkgsSrc}/nixos/modules/installer/cd-dvd/channel.nix"
    ];

    networking.networkmanager.enable = true;
    networking.useDHCP = false;
    networking.firewall.enable = false;
    networking.wireless.enable = pkgs.lib.mkForce false;
  };
in (pkgs.nixos {
  inherit configuration;
}).config.system.build.isoImage
