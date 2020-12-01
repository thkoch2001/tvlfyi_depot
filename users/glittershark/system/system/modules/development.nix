{ config, lib, pkgs, ... }:

{
  virtualisation.docker.enable = true;
  users.users.grfn.extraGroups = [ "docker" ];
}
