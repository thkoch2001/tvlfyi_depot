{ config, lib, pkgs, ... }:

{
  virtualisation.podman = {
    enable = true;
    defaultNetwork.settings = { dns_enabled = true; };
    dockerCompat = true;
    dockerSocket.enable = true;
  };

  users.users.aspen.extraGroups = [ "docker" ];
}
