{ depot, pkgs, lib, ... }:

let
  nixpkgs = import pkgs.nixpkgsSrc {};
  home-modules = import "${nixpkgs.home-manager.src}/modules";

  home = path:
    let 
      config = import path { inherit depot pkgs; };
    in
      home-modules {
        pkgs = nixpkgs;
        configuration = config;
      };

in
  rec {
    whitby = home ./configs/whitby.nix;
    whitbyHome = whitby.activationPackage;

    meta.targets = [ "whitbyHome" ];
  }
