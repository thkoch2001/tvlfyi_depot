{ config, lib, pkgs, ... }:

{
  virtualisation.docker.enable = true;
  users.users.aspen.extraGroups = [ "docker" ];

  security.pam.loginLimits = [
    {
      domain = "aspen";
      type = "soft";
      item = "nofile";
      value = "65535";
    }
  ];
}
