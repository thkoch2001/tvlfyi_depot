# Helper functions for instantiating depot-compatible NixOS machines.
{ depot, lib, pkgs, ... }@args:

let inherit (lib) findFirst isAttrs;
in rec {
  # This provides our standard set of arguments to all NixOS modules.
  baseModule = { ... }: {
    # Ensure that pkgs == third_party.nix
    nixpkgs.pkgs = depot.third_party.nixpkgs;
    nix.nixPath =
      let
        # Due to nixpkgsBisectPath, pkgs.path is not always in the nix store
        nixpkgsStorePath =
          if lib.hasPrefix builtins.storeDir (toString pkgs.path)
          then builtins.storePath pkgs.path # nixpkgs is already in the store
          else pkgs.path; # we need to dump nixpkgs to the store either way
      in
      [
        ("nixos=" + nixpkgsStorePath)
        ("nixpkgs=" + nixpkgsStorePath)
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

  rebuild-system = rebuildSystemWith (
    # HACK: use the string of the original source to avoid copying the whole
    # depot into the store just for this
    builtins.toString depot.path.origSrc);

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

  # Systems that should be built in CI
  whitbySystem = (nixosFor depot.ops.machines.whitby).system;
  sandunySystem = (nixosFor depot.ops.machines.sanduny).system;
  nixeryDev01System = (nixosFor depot.ops.machines.nixery-01).system;
  meta.ci.targets = [ "sandunySystem" "whitbySystem" "nixeryDev01System" ];
}
