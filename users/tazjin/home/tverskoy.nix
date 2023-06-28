# Home manage configuration for tverskoy.

{ depot, pkgs, ... }: # readTree
{ config, lib, ... }: # home-manager

{
  imports = [
    depot.users.tazjin.home.shared
    depot.users.tazjin.home.persistence
  ];

  home.persistence."/persist/tazjin/home" = {
    directories = [
      ".config/spotify"
      ".local/share/Steam"
    ];
  };
}
