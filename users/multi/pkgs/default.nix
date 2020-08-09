{ pkgs, ... }:

let
  htop = import ./htop { inherit pkgs; };
in
  [ htop ]
