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

  # for when hacking
  programs.home-manager.enable = true;
  home.stateVersion = "20.03";

  system.machine = {
    wirelessInterface = "wlp0s20f3";
    i3FontSize = 9;
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

  xdg.mimeApps.defaultApplications."x-scheme-handler/tg" =
    "telegramdesktop.desktop";

  programs.zsh.shellAliases = {
    "graph" = "curl -s localhost:6033/graph | dot -Tpng | feh -";
  };

  programs.ssh.matchBlocks."grfn-dev" = {
    host = "grfn-dev";
    forwardAgent = true;
    user = "ubuntu";
  };
}
