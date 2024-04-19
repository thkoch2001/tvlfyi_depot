{ config, lib, pkgs, ... }:

# Things that only work in the presence of a linux desktop environment

{
  imports = [
    ./i3.nix
    ./alacritty.nix
  ];

  home.packages = with pkgs; [
    (writeShellApplication {
      name = "edit-input";

      runtimeInputs = [ xdotool xclip ];
      text = ''
        set -euo pipefail

        sleep 0.2
        xdotool key ctrl+a ctrl+c
        xclip -out -selection clipboard > /tmp/EDIT
        emacsclient -c /tmp/EDIT
        xclip -in -selection clipboard < /tmp/EDIT
        sleep 0.2
        xdotool key ctrl+v
        rm /tmp/EDIT
      '';
    })
  ];

  services.syncthing.tray.enable = true;

  gtk = {
    enable = true;
    gtk3.bookmarks = [
      "file:///home/aspen/code"
      "file:///home/aspen/notes"
    ];
  };
}
