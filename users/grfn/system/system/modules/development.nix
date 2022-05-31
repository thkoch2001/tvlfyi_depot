{ config, lib, pkgs, ... }:

{
  virtualisation.docker.enable = true;
  users.users.grfn.extraGroups = [ "docker" ];

  security.pam.loginLimits = [
    {
      domain = "grfn";
      type = "soft";
      item = "nofile";
      value = "65535";
    }
  ];
}
