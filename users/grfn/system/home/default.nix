{ pkgs, depot, lib, ... }:

with lib;

rec {
  home = confPath: (import "${pkgs.home-manager.src}/modules" {
    inherit pkgs;

    configuration = { config, lib, ... }: {
      imports = [ confPath ];
      lib.depot = depot;

      # home-manager exposes no API to override the package set that
      # is used, unless called from the NixOS module.
      #
      # To get around it, the module argument is overridden here.
      _module.args.pkgs = mkForce pkgs;
    };
  });

  dobharchu = home ./machines/dobharchu.nix;

  dobharchuHome = dobharchu.activation-script;

  yeren = home ./machines/yeren.nix;

  yerenHome = yeren.activation-script;

  meta.targets = [
    "yerenHome"
  ];
}
