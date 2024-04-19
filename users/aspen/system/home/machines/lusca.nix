{ pkgs, lib, config, ... }:

let
  inherit (builtins) pathExists;
in
{
  imports = [
    ../platforms/linux.nix
    ../modules/common.nix

    ../modules/email.nix
    ../modules/desktop.nix
  ] ++ (lib.optional (pathExists ../modules/private.nix)
    ../modules/private.nix);

  home.username = lib.mkForce "aspen";
  home.homeDirectory = lib.mkForce "/home/aspen";

  # for when hacking
  programs.home-manager.enable = true;
  home.stateVersion = "20.03";

  system.machine = {
    wirelessInterface = "wlp1s0";
    i3FontSize = 9;
    battery = 1;
  };

  programs.alacritty.settings.font.size = lib.mkForce 5.5;

  home.packages = with pkgs; [ discord steam tdesktop slack ];

  xsession.windowManager.i3.config.keybindings.XF86AudioMedia = "exec lock";
}
