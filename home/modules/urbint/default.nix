{ config, lib, pkgs, ... }:

{
  imports = [
    ./frontend.nix
    ./backend.nix
  ];
}
