# Home manage configuration for arbat.

{ depot, pkgs, ... }: # readTree
{ config, lib, ... }: # home-manager

{
  imports = [
    depot.users.tazjin.home.shared
    depot.users.tazjin.home.persistence
  ];
}
