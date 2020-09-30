{ config, lib, pkgs, ... }:

{
  imports = [
    ./xserver.nix
    ./fonts.nix
    ./sound.nix
    ./kernel.nix
  ];

  programs.nm-applet.enable = true;

  users.users.grfn.extraGroups = [
    "audio"
    "video"
  ];

  services.geoclue2.enable = true;

  powerManagement = {
    enable = true;
    cpuFreqGovernor = lib.mkDefault "powersave";
    powertop.enable = true;
  };
}
