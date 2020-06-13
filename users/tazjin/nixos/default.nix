# TODO(tazjin): Generalise this and move to //ops/nixos
{ depot, lib, ... }:

let
  inherit (builtins) foldl';

  systemFor = configs: (depot.third_party.nixos {
    configuration = lib.fix(config:
      foldl' lib.recursiveUpdate {} (map (c: c config) configs)
    );
  }).system;

  caseFor = hostname: ''
    ${hostname})
      echo "Rebuilding NixOS for //users/tazjin/nixos/${hostname}"
      system=$(nix-build -E '(import <depot> {}).users.tazjin.nixos.${hostname}System' --no-out-link)
      ;;
  '';

  rebuilder = depot.third_party.writeShellScriptBin "rebuilder" ''
    set -ue
    if [[ $EUID -ne 0 ]]; then
      echo "Oh no! Only root is allowed to rebuild the system!" >&2
      exit 1
    fi

    case $HOSTNAME in
    ${caseFor "nugget"}
    ${caseFor "camden"}
    ${caseFor "frog"}
    *)
      echo "$HOSTNAME is not a known NixOS host!" >&2
      exit 1
      ;;
    esac

    nix-env -p /nix/var/nix/profiles/system --set $system
    $system/bin/switch-to-configuration switch
  '';
in {
  inherit rebuilder;

  nuggetSystem = systemFor [ depot.users.tazjin.nixos.nugget ];
  camdenSystem = systemFor [ depot.users.tazjin.nixos.camden ];
  frogSystem = systemFor [ depot.users.tazjin.nixos.frog ];
}
