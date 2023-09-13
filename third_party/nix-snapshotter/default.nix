# Imports the stable Nix build definitions for nix-snapshotter, a
# plugin to bring native support for Nix images to containerd
{ lib, pkgs, ... }:

let
  src = pkgs.fetchFromGitHub {
    owner = "pdtpartners";
    repo = "nix-snapshotter";
    sha256 = "11sfy3kf046p8kacp7yh8ijjpp6php6q8wxlbya1v5q53h3980v1";
    rev = "6eb21bd3429535646da4aa396bb0c1f81a9b72c6";
  };
in
pkgs.callPackage "${src}/package.nix" { }
