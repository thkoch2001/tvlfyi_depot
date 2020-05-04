{ config, lib, pkgs, ... }:

{
  imports = [
    ./common.nix
  ];

  home.packages = with pkgs; [
    yarn
    nodejs
    clojure
    nodePackages.prettier
  ];
}
