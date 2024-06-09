# arbat is my Unchartevice 6640MA, with a Zhaoxin CPU.
{ depot, lib, pkgs, ... }:

config:
{
  imports = [
    "${pkgs.path}/nixos/modules/installer/cd-dvd/installation-cd-base.nix"
  ];
  tvl.cache.enable = true;

  environment.etc = {
    "depot" = {
      source = depot.path.outPath;
      mode = "0655";
    };

    "arbat".source = depot.users.tazjin.nixos.arbatSystem;
  };

  networking.networkmanager.enable = true;
  networking.wireless.enable = false;
}
