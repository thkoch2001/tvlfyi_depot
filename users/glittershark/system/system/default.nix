{ depot, pkgs, ... }:

rec {
  chupacabra = import ./machines/chupacabra.nix;

  chupacabraSystem = (pkgs.nixos {
    configuration = chupacabra;
  }).system;

  # Build chupacabra in CI
  meta.targets = [ "chupacabraSystem" ];

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
