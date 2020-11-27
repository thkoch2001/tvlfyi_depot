{ depot, pkgs, ... }:

let

  configuration = { ... }: {
    imports = [
      "${pkgs.nixpkgsSrc}/nixos/modules/installer/cd-dvd/installation-cd-minimal.nix"
    ];

    nix.package = depot.third_party.nix;

    virtualisation.qemu.options = [ "-nographic" "-curses" ];

    nix.nixPath = [
      "depot=${depot.depotPath}"
    ];
  };

  system = pkgs.nixos { inherit configuration; };

in system.vm
