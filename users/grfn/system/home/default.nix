{ pkgs, depot, lib, ... }:

with lib;

rec {
  home = confPath: (import "${pkgs.home-manager.src}/modules" {
    inherit pkgs;
    configuration = { config, lib, ... }: {
      imports = [ confPath ];
      lib.depot = depot;
      useNixpkgsModule = false;
    };
  });

  chupacabra = home ./machines/chupacabra.nix;

  chupacabraHome = chupacabra.activation-script;

  dobharchu = home ./machines/dobharchu.nix;

  dobharchuHome = dobharchu.activation-script;

  yeren = home ./machines/yeren.nix;

  yerenHome = yeren.activation-script;

  meta.targets = [
    "chupacabraHome"
    "yerenHome"
  ];
}
