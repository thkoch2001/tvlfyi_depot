{ depot, pkgs, lib, ... }:

let
  inherit (depot.users.flokli.nixos)
    archeology;

  systemFor = sys: (depot.ops.nixos.nixosFor sys).system;

  # assumes `name` is configured appropriately in your .ssh/config
  deployScript = name: sys: pkgs.writeShellScriptBin "deploy-${name}" ''
    set -eo pipefail
    nix copy --no-check-sigs --to ssh-ng://${name} ${sys}
    ssh ${name} nix-env --profile /nix/var/nix/profiles/system --set ${sys}
    ssh ${name} ${sys}/bin/switch-to-configuration switch
  '';

in
rec {
  archeologySystem = (depot.ops.nixos.nixosFor ({ ... }: {
    imports = [
      ./archeology/configuration.nix
    ];
  })).config.system.build.toplevel;

  shell = pkgs.mkShell {
    name = "flokli-nixos-shell";
    packages = [
      (deployScript "archeology" archeologySystem)
    ];
  };
}
