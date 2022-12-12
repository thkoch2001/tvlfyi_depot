{ depot, lib, pkgs, ... }:

let
  configuration = { ... }: {
    imports = [
      (pkgs.path + "/nixos/modules/installer/cd-dvd/installation-cd-minimal.nix")
      (pkgs.path + "/nixos/modules/installer/cd-dvd/channel.nix")
    ];

    networking.networkmanager.enable = true;
    networking.useDHCP = false;
    networking.firewall.enable = false;
    networking.wireless.enable = lib.mkForce false;

    # TODO(grfn): enabling this (in the minimal profile) fails the iso build,
    # since gtk+3 needs to be built which fails due to cairo without xlibs
    environment.noXlibs = false;
  };
in
(depot.third_party.nixos {
  inherit configuration;
}).config.system.build.isoImage
