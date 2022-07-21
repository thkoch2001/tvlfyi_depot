{ pkgs, lib, config, ... }:

let
  inherit (builtins) pathExists;
  laptopKeyboardId = "5";
in

{
  imports = [
    ../platforms/linux.nix
    ../modules/common.nix
    ../modules/desktop.nix
    ../modules/development/agda.nix
    ../modules/development/readyset.nix
  ] ++ (lib.optional (pathExists ../modules/private.nix) ../modules/private.nix);

  programs.home-manager.enable = true;
  home.stateVersion = "21.11";

  system.machine = {
    wirelessInterface = "wlp4s0";
    i3FontSize = 9;
    battery = false;
  };

  home.packages = with pkgs; [
    zoom-us
    slack
    mysql
    graphviz
    gnuplot
    mypaint
    xdot
    tdesktop
    subsurface
    discord
    steam
  ];

  systemd.user.services.laptop-keyboard = {
    Unit = {
      Description = "Swap caps+escape and alt+super, but only on the built-in laptop keyboard";
      After = [ "graphical-session-pre.target" ];
      PartOf = [ "graphical-session.target" ];
    };

    Install = { WantedBy = [ "graphical-session.target" ]; };

    Service = {
      Type = "oneshot";
      RemainAfterExit = true;
      ExecStart = (
        "${pkgs.xorg.setxkbmap}/bin/setxkbmap "
        + "-device ${laptopKeyboardId} "
        + "-option caps:swapescape "
        + "-option compose:ralt "
        + "-option altwin:swap_alt_win"
      );
    };
  };

  xsession.windowManager.i3.config.keybindings.F9 = "exec lock";

  # Telegram adds this to ~/.config/mimeapps.list if it isn't already there,
  # preventing home manager from installing (since it doesn't want to overwrite
  # the file)
  xdg.mimeApps.defaultApplications."x-scheme-handler/tg" =
    "userapp-Telegram Desktop-K290F1.desktop";
}
