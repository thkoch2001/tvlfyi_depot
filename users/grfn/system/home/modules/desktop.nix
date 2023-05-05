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
