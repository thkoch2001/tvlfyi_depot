{ config, lib, pkgs, ... }:

# Things that only work in the presence of a linux desktop environment

{
  imports = [
    ./i3.nix
    ./obs.nix
    ./games.nix
  ];

  home.packages = with pkgs; [
    ntfy
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
