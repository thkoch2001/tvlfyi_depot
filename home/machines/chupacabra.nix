{ pkgs, ... }:
let
  laptopKeyboardId = "25";
in {
  imports = [
    ../platforms/linux.nix

    ../modules/common.nix
    ../modules/games.nix
  ];

  system.machine = {
    wirelessInterface = "wlp59s0";
    i3FontSize = 9;
  };

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
}
