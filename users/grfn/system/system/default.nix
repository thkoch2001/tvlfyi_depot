args @ { depot, pkgs, ... }:

rec {
  chupacabra = import ./machines/chupacabra.nix;

  chupacabraSystem = (depot.third_party.nixos {
    configuration = chupacabra;
  }).system;

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

  # Build chupacabra in CI
  meta.targets = [
    "chupacabraSystem"
    "mugwumpSystem"
    "roswellSystem"
    "yerenSystem"

    "iso"
  ];

  rebuilder =
    let
      depotPath = "/home/grfn/code/depot";

      caseFor = hostname: ''
        ${hostname})
          echo "Rebuilding NixOS for //users/grfn/nixos/${hostname}"
          system=$(nix-build -E '(import ${depotPath} {}).users.grfn.system.system.${hostname}' --no-out-link)
          ;;
      '';
    in pkgs.writeShellScriptBin "rebuilder" ''
      set -ue
      if [[ $EUID -ne 0 ]]; then
        echo "Oh no! Only root is allowed to rebuild the system!" >&2
        exit 1
      fi

      case $HOSTNAME in
      ${caseFor "chupacabra"}
      *)
        echo "$HOSTNAME is not a known NixOS host!" >&2
        exit 1
        ;;
      esac

      nix-env -p /nix/var/nix/profiles/system --set $system
      $system/bin/switch-to-configuration switch
    '';
}
