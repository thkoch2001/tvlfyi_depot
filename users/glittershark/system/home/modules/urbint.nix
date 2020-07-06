{ config, lib, pkgs, ... }:

if lib.pathExists (~/code/urb/urbos)
then {
  imports =
    [ ~/code/urb/urbos/home ];

  urbint.projectPath = "code/urb";
} else {}
