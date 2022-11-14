{ depot, pkgs, ... }:

let
  inherit (depot.users.wpcarro.nixos)
    ava
    diogenes
    kyoko
    marcus
    tarasco;

  systemFor = sys: (depot.ops.nixos.nixosFor sys).system;
in
{
  avaSystem = systemFor ava;
  kyokoSystem = systemFor kyoko;
  marcusSystem = systemFor marcus;
  tarascoSystem = systemFor ava;

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

    function cleanup() {
      rm -f "$TF_STATE_DIR/$(basename $STORE_PATH)"
    }
    trap cleanup EXIT

    ${pkgs.terraform}/bin/terraform -chdir="$TF_STATE_DIR" apply
  '';

  # Rebuild NixOS for diogenes without applying terraform updates.
  rebuild-diogenes = pkgs.writeShellScriptBin "rebuild-diogenes" ''
    set -euo pipefail
    readonly target="root@billandhiscomputer.com"

    # We need to call nix-build here on the drvPath because it may not be in
    # /nix/store yet.
    readonly STORE_PATH="$(nix-build ${diogenes.drvPath} --no-out-link --show-trace)"
    nix-copy-closure --to $target ${diogenes.osPath} \
      --gzip --use-substitutes $STORE_PATH

    ssh $target 'nix-env --profile /nix/var/nix/profiles/system --set ${diogenes.osPath}'
    ssh $target '${diogenes.osPath}/bin/switch-to-configuration switch'
  '';

  meta.ci.targets = [
    "avaSystem"
    "kyokoSystem"
    "marcusSystem"
    "tarascoSystem"
  ];
}
