{ pkgs, depot, lib, ... }:

with lib;

rec {
  nixpkgs = import pkgs.nixpkgsSrc {};

  home-manager = (fetchTarball {
    url = "https://github.com/rycee/home-manager/archive/152769aed96d4d6f005ab40daf03ec4f5102c763.tar.gz";
    sha256 = "10svwspmsf46rijzsh0h9nmz1mq2998wcml8yp36mwksgi8695pc";
  });

  home = confPath: (import "${home-manager}/modules" {
    pkgs = nixpkgs;
    configuration = { config, lib, ... }: {
      imports = [confPath];

      _module.args.pkgs = mkForce
        (import pkgs.nixpkgsSrc (filterAttrs (n: v: v != null) config.nixpkgs));

      lib.depot = depot;
    };
  }) // { __readTree = true; };

  chupacabra = home ./machines/chupacabra.nix;
}
