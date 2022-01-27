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

  rebuild-system = pkgs.writeShellScriptBin "rebuild-system" ''
    set -ue
    if [[ $EUID -ne 0 ]]; then
      echo "Oh no! Only root is allowed to rebuild the system!" >&2
      exit 1
    fi

    readonly depot_path=''${1:-${toString depot.path.origSrc}}

    echo "Rebuilding NixOS for $HOSTNAME"
    system=$(${pkgs.nix}/bin/nix-build -E "((import \"$depot_path\" {}).ops.nixos.findSystem \"$HOSTNAME\").system" --no-out-link --show-trace)
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
    readonly build=$state_directory/build
    readonly git="${pkgs.git}/bin/git -C $depot"

    mkdir -p $state_directory

    # find-or-create depot
    if [ ! -d $depot ]; then
      # cannot use $git here because $depot doesn't exist
      ${pkgs.git}/bin/git clone --bare $git_remote $depot
    fi

    function cleanup() {
      $git worktree remove $build
    }
    trap cleanup EXIT

    $git fetch origin
    $git worktree add --force $build FETCH_HEAD
    # unsure why, but without this switch-to-configuration attempts to install
    # NixOS in $state_directory
    (cd / && ${rebuild-system}/bin/rebuild-system $build)
  '';

  # Systems that should be built in CI
  whitbySystem = (nixosFor depot.ops.machines.whitby).system;
  meta.targets = [ "whitbySystem" ];
}
