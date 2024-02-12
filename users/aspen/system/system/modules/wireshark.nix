{ config, lib, pkgs, ... }:

{
  programs.wireshark = {
    enable = true;
    package = pkgs.wireshark;
  };
  users.users.grfn.extraGroups = [ "wireshark" ];
}
