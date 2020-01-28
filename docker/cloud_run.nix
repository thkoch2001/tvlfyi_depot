# Attempting to build a Docker image with Nix to run using Google Cloud Run.
{ pkgs ? import <nixpkgs> {}, ... }:

pkgs.dockerTools.buildLayeredImage {
  name = "mysql";
  tag = "latest";
  config.Cmd = [ "${pkgs.mysql}/bin/mysqld" ];
  maxLayers = 120;
}
