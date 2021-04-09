{ pkgs, depot, lib, ... }:

with lib;

rec {
  home = confPath: (import "${pkgs.home-manager.src}/modules" {
    inherit pkgs;
    configuration = { config, lib, ... }: {
      imports = [confPath];

      _module.args.pkgs = mkForce
        (import depot.third_party.nixpkgsSrc (filterAttrs (n: v: v != null) config.nixpkgs));

      lib.depot = depot;
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
