{ depot, pkgs, ... }:

let
  configuration = { ... }: {
    imports = [
      "${depot.third_party.nixpkgsSrc}/nixos/modules/installer/cd-dvd/installation-cd-minimal.nix"
    ];

    nix.package = depot.third_party.nix;

    virtualisation.qemu.options = [ "-nographic" "-curses" ];

    nix.nixPath = [
      "depot=${depot.depotPath}"
    ];
  };

  system = depot.third_party.partialNixos { inherit configuration; };
in system.vm
