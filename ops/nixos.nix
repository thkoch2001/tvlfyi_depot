# Helper functions for instantiating depot-compatible NixOS machines.
{ depot
, lib
, pkgs
, ...
}
@ args:
let
  inherit ( lib ) findFirst isAttrs;
in
rec
  {
  # This provides our standard set of arguments to all NixOS modules.
  baseModule =
    { ...
    }:
    {
      # Ensure that pkgs == third_party.nix
      nixpkgs.pkgs = depot.third_party.nixpkgs;
      nix.nixPath = [ "nixos=${ pkgs.path }" "nixpkgs=${ pkgs.path }" ];
    };
  nixosFor =
    configuration:
    (
      depot.third_party.nixos
        {
          configuration = { ... }: { imports = [ baseModule configuration ]; };
          specialArgs = { inherit ( args ) depot; };
        }
    );
  findSystem =
    hostname:
    (
      findFirst
        ( system: system.config.networking.hostName == hostname )
        ( throw "${ hostname } is not a known NixOS host" )
        ( map nixosFor depot.ops.machines.all-systems )
    );
  rebuild-system = rebuildSystemWith depot.path;
  rebuildSystemWith =
    depotPath:
    pkgs.writeShellScriptBin
      "rebuild-system"
      ''
      set -ue
      if [[ $EUID -ne 0 ]]; then
        echo "Oh no! Only root is allowed to rebuild the system!" >&2
        exit 1
      fi

      echo "Rebuilding NixOS for $HOSTNAME"
      system=$(${ pkgs.nix }/bin/nix-build -E "((import ${ depotPath } {}).ops.nixos.findSystem \"$HOSTNAME\").system" --no-out-link --show-trace)

      ${ pkgs.nix }/bin/nix-env -p /nix/var/nix/profiles/system --set $system
      $system/bin/switch-to-configuration switch
      '';
  # Systems that should be built in CI
  whitbySystem = ( nixosFor depot.ops.machines.whitby ).system;
  meta.targets = [ "whitbySystem" ];
}
