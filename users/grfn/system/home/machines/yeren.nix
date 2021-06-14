{ pkgs, lib, config, ... }:

let
  laptopKeyboardId = "5";
in

{
  imports = [
    ../platforms/linux.nix
    ../modules/common.nix
    ../modules/development/readyset.nix
  ];

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
      version = "0.0.15";
      src = fetchurl {
        url = "https://dl.discordapp.net/apps/linux/${version}/discord-${version}.tar.gz";
        sha256 = "0pn2qczim79hqk2limgh88fsn93sa8wvana74mpdk5n6x5afkvdd";
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
}
