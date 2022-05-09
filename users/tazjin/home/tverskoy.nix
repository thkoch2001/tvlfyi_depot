# Home manage configuration for tverskoy.

{ depot, pkgs, ... }: # readTree
{ config, lib, ... }: # home-manager

{
  imports = [
    depot.users.tazjin.home.shared
  ];

  home.persistence."/persist/tazjin/home" = {
    directories = [
      ".config/spotify"
      ".config/unity3d"
      ".local/share/Steam"
      ".electrum"
    ];
  };
}
