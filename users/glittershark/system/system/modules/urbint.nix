{ config, pkgs, lib, ... }:

with lib;

{
  virtualisation.docker.enable = true;

  nix = rec {
    binaryCaches = [ "https://nix.urbinternal.com" ];
    trustedBinaryCaches = binaryCaches;
    requireSignedBinaryCaches = false;
  };

  services.openvpn.servers.urbint = {
    config = ''
      config /root/openvpn/urbint.conf
    '';
    autoStart = false;
  };

  users.users."grfn".extraGroups = [ "docker" ];

  services.clamav = {
    daemon.enable = true;
    updater = {
      enable = true;
      interval = "daily";
      frequency = 1; # per day
    };
  };
}
