# Helper functions for instantiating depot-compatible NixOS machines.
{ depot, lib, pkgs, ... }@args:

let inherit (lib) findFirst isAttrs;
in rec {
  # This provides our standard set of arguments to all NixOS modules.
  baseModule = { ... }: {
    # Ensure that pkgs == third_party.nix
    nixpkgs.pkgs = depot.third_party.nixpkgs;
    nix.nixPath = [
      "nixos=${pkgs.path}"
      "nixpkgs=${pkgs.path}"
    ];
  };

  nixosFor = configuration: (depot.third_party.nixos {
    configuration = { ... }: {
      imports = [
        baseModule
        configuration
      ];
    };

    specialArgs = {
      inherit (args) depot;
    };
  });

  findSystem = hostname:
    (findFirst
      (system: system.config.networking.hostName == hostname)
      (throw "${hostname} is not a known NixOS host")
      (map nixosFor depot.ops.machines.all-systems));

  rebuild-system = rebuildSystemWith depot.path;

  rebuildSystemWith = depotPath: pkgs.writeShellScriptBin "rebuild-system" ''
    set -ue
    if [[ $EUID -ne 0 ]]; then
      echo "Oh no! Only root is allowed to rebuild the system!" >&2
      exit 1
    fi

    echo "Rebuilding NixOS for $HOSTNAME"
    system=$(${pkgs.nix}/bin/nix-build -E "((import ${depotPath} {}).ops.nixos.findSystem \"$HOSTNAME\").system" --no-out-link --show-trace)

    ${pkgs.nix}/bin/nix-env -p /nix/var/nix/profiles/system --set $system
    $system/bin/switch-to-configuration switch
  '';

  upgrade-system = pkgs.writeShellScriptBin "upgrade-system" ''
    set -ueo pipefail

    if [[ $EUID -ne 0 ]]; then
      echo "Oh no! Only root is allowed to run upgrade-system!" >&2
      exit 1
    fi

    readonly state_directory=''${1:-/var/lib/upgrade-system}
    readonly git_remote=''${2:-https://cl.tvl.fyi/depot.git}
    readonly depot=$state_directory/depot.git
    readonly deploy=$state_directory/deploy
    readonly git="git -C $depot"

    mkdir -p $state_directory

    # find-or-create depot
    if [ ! -d $depot ]; then
      # cannot use $git here because $depot doesn't exist
      git clone --bare $git_remote $depot
    fi

    function cleanup() {
      $git worktree remove $deploy
    }
    trap cleanup EXIT

    $git fetch origin
    $git worktree add --force $deploy FETCH_HEAD
    # unsure why, but without this switch-to-configuration attempts to install
    # NixOS in $state_directory
    (cd / && ${rebuild-system}/bin/rebuild-system)
  '';

  # Systems that should be built in CI
  whitbySystem = (nixosFor depot.ops.machines.whitby).system;
  meta.targets = [ "whitbySystem" ];
}
