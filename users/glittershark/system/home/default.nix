{ pkgs, depot, lib, ... }:

with lib;

rec {
  nixpkgs = import pkgs.nixpkgsSrc {};

  home = confPath: (import "${nixpkgs.home-manager.src}/modules" {
    pkgs = nixpkgs;
    configuration = { config, lib, ... }: {
      imports = [confPath];

      _module.args.pkgs = mkForce
        (import pkgs.nixpkgsSrc (filterAttrs (n: v: v != null) config.nixpkgs));

      lib.depot = depot;
    };
  });

  chupacabra = home ./machines/chupacabra.nix;

  chupacabraHome = chupacabra.activation-script;

  dobharchu = home ./machines/dobharchu.nix;

  dobharchuHome = dobharchu.activation-script;

  meta.targets = [ "chupacabraHome" ];
}
