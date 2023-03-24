{ config, lib, pkgs, depot, ... }:

{
  imports = [
    (depot.path + "/ops/modules/hm-nmd-workaround.nix")
    ../platforms/darwin.nix
    ../modules/common.nix
    ../modules/games.nix
  ];

  home.packages = with pkgs; [
    coreutils
    gnupg
    nix-prefetch-github
    pass
    pinentry_mac
  ];
}
