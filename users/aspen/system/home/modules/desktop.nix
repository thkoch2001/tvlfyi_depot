{ config, lib, pkgs, ... }:

# Things that only work in the presence of a linux desktop environment

{
  imports = [
    ./i3.nix
    ./obs.nix
    ./games.nix
  ];

  home.packages = with pkgs; [
    (ntfy.override {
      # Slack support is broken as of 2023-06-15
      withSlack = false;
    })
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
      "file:///home/grfn/code"
      "file:///home/grfn/notes"
    ];
  };
}
