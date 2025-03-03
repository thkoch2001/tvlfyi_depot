# Inject the right home-manager config for the machine.

{ config, depot, pkgs, ... }:

{
  users.users.tazjin = {
    isNormalUser = true;
    createHome = true;
    extraGroups = [ "wheel" "networkmanager" "video" "adbusers" "yggdrasil" ];
    uid = 1000;
    shell = pkgs.fish;
    initialHashedPassword = "$2b$05$1eBPdoIgan/C/L8JFqIHBuVscQyTKw1L/4VBlzlLvLBEf6CXS3EW6";
  };

  nix.settings.trusted-users = [ "tazjin" ];

  home-manager.backupFileExtension = "backup";
  home-manager.useGlobalPkgs = true;
  home-manager.users.tazjin = with depot.users.tazjin;
    home."${config.networking.hostName}" or home.shared;
}
