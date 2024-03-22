{ config, lib, pkgs, ... }:

{
  programs.wireshark = {
    enable = true;
    package = pkgs.wireshark;
  };
  users.users.aspen.extraGroups = [ "wireshark" ];
}
