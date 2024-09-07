# Home manage configuration for zamalek.

{ depot, pkgs, ... }: # readTree
{ config, lib, ... }: # home-manager

{
  imports = [
    depot.users.tazjin.home.shared
    depot.users.tazjin.home.persistence
  ];

  gtk.cursorTheme.name = lib.mkDefault "Chicago95_Animated_Hourglass_Cursors_HiDPI";
}
