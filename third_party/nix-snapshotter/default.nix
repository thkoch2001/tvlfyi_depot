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

# Definition doesn't work with current nixpkgs due to use of removed vendorSha256.
  # This could be fixed by updating, but it is unclear to me (sterni) if the pinned
  # commit was chosen for any specific reason.
depot.nix.readTree.skipTarget (pkgs.callPackage "${src}/package.nix" { })
