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

    home-manager.users.aspen = { config, lib, ... }: {
      imports = [ ../home/machines/roswell.nix ];
      lib.depot = depot;
    };
  })).system;

  ogopogo = import ./machines/ogopogo.nix;

  ogopogoSystem = (depot.ops.nixos.nixosFor ogopogo).system;

  yeren = import ./machines/yeren.nix;

  yerenSystem = (depot.ops.nixos.nixosFor yeren).system;

  lusca = import ./machines/lusca.nix;

  luscaSystem = (depot.ops.nixos.nixosFor lusca).system;

  # TODO(aspen): reenable after
  # https://github.com/NixOS/nixpkgs/pull/234883 has propagated
  # through to our channel
  # iso = import ./iso.nix args;

  meta.ci.targets = [
    "mugwumpSystem"
    "roswellSystem"
    "ogopogoSystem"
    "yerenSystem"

    "iso"
  ];
}
