{ depot, pkgs, ... }:

let 
  nixpkgs = import pkgs.nixpkgsSrc {};
  localpkg = path: import path { pkgs = nixpkgs; };

  packages = {
    htop = localpkg ./htop;
  };
in
  packages // {
    meta.targets = builtins.attrNames packages;
  }
