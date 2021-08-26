args @ { depot, pkgs, ... }:

rec {
  mugwump = import ./machines/mugwump.nix;

  mugwumpSystem = (depot.ops.nixos.nixosFor mugwump).system;

  roswell = import ./machines/roswell.nix;

  roswellSystem = (depot.ops.nixos.nixosFor ({ ... }: {
    imports = [
      ./machines/roswell.nix
      "${pkgs.home-manager.src}/nixos"
    ];

    # Use the same nixpkgs as everything else
    home-manager.useGlobalPkgs = true;

    home-manager.users.grfn = { config, lib, ... }: {
      imports = [ ../home/machines/roswell.nix ];
      lib.depot = depot;
    };
  })).system;

  yeren = import ./machines/yeren.nix;

  yerenSystem = (depot.ops.nixos.nixosFor yeren).system;

  iso = import ./iso.nix args;

  meta.targets = [
    "mugwumpSystem"
    "roswellSystem"
    "yerenSystem"

    "iso"
  ];
}
