{ config, lib, pkgs, ... }:

{
  programs.wireshark.enable = true;
  users.users.grfn.extraGroups = [ "wireshark" ];
}
