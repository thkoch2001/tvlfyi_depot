{ config, lib, pkgs, ... }:

let

  pttKeycode = "152";
  sourceID = "3";

  mute = pkgs.writeShellScript "mute-mic" ''
    xset -r ${pttKeycode}
    ${pkgs.pulseaudio}/bin/pactl set-source-mute ${sourceID} 1
  '';

  unmute = pkgs.writeShellScript "unmute-mic" ''
    xset -r ${pttKeycode}
    ${pkgs.pulseaudio}/bin/pactl set-source-mute ${sourceID} 0
  '';

in

{
  home.packages = with pkgs; [
    xbindkeys
  ];


  home.file.".xbindkeysrc.scm".text = ''
    (xbindkey '("c:${pttKeycode}") "${unmute}")
    (xbindkey '(release "c:${pttKeycode}") "${mute}")
  '';

  systemd.user.services."xbindkeys" = {
    Unit = {
      Description = "Keybind daemon for push-to-talk";
      After = [ "graphical-session-pre.target" ];
      PartOf = [ "graphical-session.target" ];
    };

    Install = { WantedBy = [ "graphical-session.target" ]; };

    Service = {
      ExecStart = "${pkgs.xbindkeys}/bin/xbindkeys -n -v";
    };
  };
}
