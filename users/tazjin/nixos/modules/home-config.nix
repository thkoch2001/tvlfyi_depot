# Inject the right home-manager config for the machine.

{ config, depot, pkgs, ... }:

{
  users.users.tazjin = {
    isNormalUser = true;
    createHome = true;
    extraGroups = [ "wheel" "networkmanager" "video" "adbusers" ];
    uid = 1000;
    shell = pkgs.fish;
    initialHashedPassword = "$6$d3FywUNCuZnJ4l.$ZW2ul59MLYon1v1xhC3lTJZfZ91lWW6Tpi13MpME0cJcYZNrsx7ABdgQRn.K05awruG2Y9ARAzURnmiJ31WTS1h";
  };

  nix = {
    trustedUsers = [ "tazjin" ];
  };

  home-manager.useGlobalPkgs = true;
  home-manager.users.tazjin = depot.users.tazjin.home."${config.networking.hostName}";
}
