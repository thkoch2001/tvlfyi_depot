# Home manage configuration for zamalek.

{ depot, pkgs, ... }: # readTree
{ config, lib, ... }: # home-manager

{
  imports = [
    depot.users.tazjin.home.shared
  ];
}
