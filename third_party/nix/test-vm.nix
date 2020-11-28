{ depot, pkgs, ... }:

let

  configuration = { ... }: {
    imports = [
      "${pkgs.nixpkgsSrc}/nixos/modules/installer/cd-dvd/installation-cd-minimal.nix"
    ];

    nix.package = depot.third_party.nix;

    environment.systemPackages = [
      pkgs.gdb
      pkgs.git
    ];

    virtualisation.qemu.options = [ "-nographic" "-curses" ];
    virtualisation.memorySize = 2048;
    virtualisation.diskSize = 2048;

    nix.nixPath = [
      "depot=${depot.depotPath}"
    ];
  };

  system = pkgs.nixos { inherit configuration; };

in system.vm
