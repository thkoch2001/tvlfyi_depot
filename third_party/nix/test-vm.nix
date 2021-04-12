{ depot, pkgs, ... }:

let
  configuration = { ... }: {
    imports = [
      "${pkgs.path}/nixos/modules/installer/cd-dvd/installation-cd-minimal.nix"
    ];

    nix.package = depot.third_party.nix;

    virtualisation.qemu.options = [ "-nographic" "-curses" ];

    nix.nixPath = [
      "depot=${depot.path}"
    ];
  };

  system = depot.third_party.nixos { inherit configuration; };
in system.vm
