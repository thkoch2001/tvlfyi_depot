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

  users.users = listToAttrs [{
    name = config.urbos.username;
    value = {
      extraGroups = [ "docker" ];
    };
  }];

  services.clamav = {
    daemon.enable = true;
    updater = {
      enable = true;
      interval = "daily";
      frequency = 1; # per day
    };
  };
}
