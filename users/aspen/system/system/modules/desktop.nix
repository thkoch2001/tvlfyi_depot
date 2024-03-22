{ config, lib, pkgs, ... }:

{
  imports = [
    ./xserver.nix
    ./fonts.nix
    ./sound.nix
    ./kernel.nix
  ];

  programs.nm-applet.enable = true;

  users.users.aspen.extraGroups = [
    "audio"
    "video"
  ];

  services.geoclue2.enable = true;
}
