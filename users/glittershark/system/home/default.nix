{ pkgs, depot, lib, ... }:

with lib;

rec {
  nixpkgs = import pkgs.nixpkgsSrc {};

  home-manager = (fetchTarball {
    url = "https://github.com/rycee/home-manager/archive/5f189acce44dc39ea4055bfd8064adaf90d7fb5a.tar.gz";
    sha256 = "0ibmvg3k9m9yzh8ln3jlh47nrvgg81iy8gpl112wjimlp6gagxw6";
  });

  home = confPath: import "${home-manager}/modules" {
    pkgs = nixpkgs;
    configuration = { config, lib, ... }: {
      imports = [confPath];

      _module.args.pkgs = mkForce
        (import pkgs.nixpkgsSrc (filterAttrs (n: v: v != null) config.nixpkgs));
    };
  };

  chupacabra = home ./machines/chupacabra.nix;
}
