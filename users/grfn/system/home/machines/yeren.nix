{ pkgs, lib, config, ... }:

let
  inherit (builtins) pathExists;
  laptopKeyboardId = "5";
in

{
  imports = [
    ../platforms/linux.nix
    ../modules/common.nix
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
    mypaint
    xdot
    tdesktop
    subsurface

    (discord.override rec {
      version = "0.0.16";
      src = fetchurl {
        url = "https://dl.discordapp.net/apps/linux/${version}/discord-${version}.tar.gz";
        sha256 = "1s9qym58cjm8m8kg3zywvwai2i3adiq6sdayygk2zv72ry74ldai";
      };
    })

    steam

    awscli2
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
}
