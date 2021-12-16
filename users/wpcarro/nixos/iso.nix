# TODO(wpcarro): Support the workflow outlined in these docs.
#
# Usage:
#   $ lsblk  # get your USB dev path (e.g. /dev/sdb)
#   $ create-installer --dev=/dev/sdb //users/wpcarro/nixos/marcus

{ pkgs, ... }:

{
  imports = [
    "${pkgs.nixos}/modules/installer/cd-graphical-gnome.nix"
  ];

  config = {
    networking.wireless.enable = true;
  };
}
