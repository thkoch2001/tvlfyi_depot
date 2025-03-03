{ config, lib, pkgs, ... }:

{
  imports = [
    ../platforms/darwin.nix
    ../modules/common.nix
    # ../modules/games.nix
  ];

  home.packages = with pkgs; [
    coreutils
    gnupg
    nix-prefetch-github
    pass
    pinentry_mac
  ];

  programs.home-manager.enable = true;
  home.stateVersion = "21.11";
}
