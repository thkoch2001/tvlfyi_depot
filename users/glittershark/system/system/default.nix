args @ { depot, pkgs, ... }:

let
  nixpkgs = import pkgs.nixpkgsSrc {};
in

rec {
  chupacabra = import ./machines/chupacabra.nix;

  chupacabraSystem = (pkgs.nixos {
    configuration = chupacabra;
  }).system;

  mugwump = import ./machines/mugwump.nix;

  mugwumpSystem = (pkgs.nixos {
    configuration = mugwump;
  }).system;

  roswell = import ./machines/roswell.nix;

  roswellSystem = (pkgs.nixos {
    configuration = { ... }: {
      imports = [
        ./machines/roswell.nix
        "${nixpkgs.home-manager.src}/nixos"
        "${depot.depotPath}/ops/nixos/depot.nix"
      ];
      inherit depot;

      home-manager.users.grfn = { config, lib, ... }: {
        imports = [ ../home/machines/roswell.nix ];
        lib.depot = depot;
        _module.args.pkgs = lib.mkForce
          (import pkgs.nixpkgsSrc
            (lib.filterAttrs (n: v: v != null) config.nixpkgs));
      };
    };
  }).system;

  yeren = import ./machines/yeren.nix;

  yerenSystem = (pkgs.nixos {
    configuration = { ... }: {
      imports = [
        ./machines/yeren.nix
        "${depot.depotPath}/ops/nixos/depot.nix"
      ];
      inherit depot;
    };
  }).system;

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
          echo "Rebuilding NixOS for //users/glittershark/nixos/${hostname}"
          system=$(nix-build -E '(import ${depotPath} {}).users.glittershark.system.system.${hostname}' --no-out-link)
          ;;
      '';
    in depot.third_party.writeShellScriptBin "rebuilder" ''
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
