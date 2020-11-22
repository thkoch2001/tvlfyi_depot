{ depot, pkgs, lib, ... }:

let
  nixpkgs = import pkgs.nixpkgsSrc {};
  home-modules = import "${nixpkgs.home-manager.src}/modules";
  outerLib = lib;

  home = path:
    let 
      configSet = import path { inherit depot pkgs; };
    in
      home-modules {
        pkgs = nixpkgs;
        configuration = { config, lib, ... }: (
          {
            _module.args.pkgs = import pkgs.nixpkgsSrc {};
          } // configSet
        );
      };

in
  rec {
    whitby = home ./configs/whitby.nix;
    whitbyHome = whitby.activationPackage;

    meta.targets = [ "whitbyHome" ];
  }
