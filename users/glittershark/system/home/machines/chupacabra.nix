{ pkgs, lib, config, ... }:

with lib;

let
  laptopKeyboardId = "25";
in {
  imports = [
    ../platforms/linux.nix
    ../modules/common.nix
    ../modules/games.nix
    ../modules/rtlsdr.nix
    ../modules/ptt.nix
  ];

  # for when hacking
  programs.home-manager.path = "/home/grfn/code/home-manager";
  programs.home-manager.enable = true;
  home.stateVersion = "19.09";

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

  home.packages = with pkgs; [
    steam
    xorg.libxcb
    (writeShellScriptBin "rebuild-mugwump" ''
      set -eo pipefail
      cd ~/code/depot
      nix build -f . users.glittershark.system.system.mugwumpSystem -o mugwump
      nix copy -f . users.glittershark.system.system.mugwumpSystem \
        --to ssh://mugwump
      system=$(readlink -ef mugwump)
      ssh mugwump sudo nix-env -p /nix/var/nix/profiles/system --set $system
      ssh mugwump sudo $system/bin/switch-to-configuration switch
    '')
  ];
}
