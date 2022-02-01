{ depot, pkgs, ... }:

let
  inherit (depot.users.wpcarro.nixos) diogenes;
  systemFor = sys: (depot.ops.nixos.nixosFor sys).system;
in
{
  marcusSystem = systemFor depot.users.wpcarro.nixos.marcus;

  # Apply terraform updates and rebuild NixOS for diogenes.
  deploy-diogenes = pkgs.writeShellScriptBin "deploy-diogenes" ''
    set -euo pipefail
    readonly TF_STATE_DIR=/depot/users/wpcarro/terraform
    rm -f $TF_STATE_DIR/*.json
    readonly STORE_PATH="${diogenes.json}"
    # We can't use the result symlink because terraform looks for a *.json file
    # in the current working directory.
    cp $STORE_PATH $TF_STATE_DIR

    if [ ! -d $TF_STATE_DIR/.terraform ]; then
      ${pkgs.terraform}/bin/terraform -chdir="$TF_STATE_DIR" init
    fi

    # function cleanup() {
    #   rm -f "$TF_STATE_DIR/$(basename $STORE_PATH)"
    # }
    # trap cleanup EXIT

    ${pkgs.terraform}/bin/terraform -chdir="$TF_STATE_DIR" apply
  '';

  # Rebuild NixOS for diogenes without applying terraform updates.
  rebuild-diogenes = pkgs.writeShellScriptBin "rebuild-diogenes" ''
    set -euo pipefail
    readonly target="root@''${1}"

    # We need to call nix-build here on the drvPath because it may not be in
    # /nix/store yet.
    readonly STORE_PATH="$(nix-build ${diogenes.drvPath} --no-out-link --show-trace)"
    nix-copy-closure --to $target ${diogenes.osPath} \
      --gzip --use-substitutes $STORE_PATH

    ssh $target 'nix-env --profile /nix/var/nix/profiles/system --set ${diogenes.osPath}'
    ssh $target '${diogenes.osPath}/bin/switch-to-configuration switch'
  '';

  meta.targets = [ "marcusSystem" ];
}
