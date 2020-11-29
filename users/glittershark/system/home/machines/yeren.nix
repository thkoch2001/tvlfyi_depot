{ pkgs, lib, config, ... }:

{
  imports = [
    ../platforms/linux.nix
    ../modules/common.nix
  ];

  # for when hacking
  programs.home-manager.enable = true;
  home.stateVersion = "20.03";

  system.machine = {
    wirelessInterface = "wlp0s20f3";
    i3FontSize = 9;
  };
}
