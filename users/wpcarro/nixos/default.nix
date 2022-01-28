{ depot
, pkgs
, ...
}:
let
  systemFor = sys: ( depot.ops.nixos.nixosFor sys ).system;
in
{
  marcusSystem = systemFor depot.users.wpcarro.nixos.marcus;
  deploy-diogenes =
    pkgs.writeShellScriptBin
      "deploy-diogenes"
      ''
      set -euo pipefail
      readonly TF_STATE_DIR=/depot/users/wpcarro/terraform
      rm -f $TF_STATE_DIR/*.json
      readonly STORE_PATH="$(nix-build /depot -A users.wpcarro.nixos.diogenes)"
      cp $STORE_PATH $TF_STATE_DIR

      function cleanup() {
        rm -f "$TF_STATE_DIR/$(basename $STORE_PATH)"
      }

      trap cleanup EXIT
      ${ pkgs.terraform }/bin/terraform -chdir="$TF_STATE_DIR" apply
      '';
  meta.targets = [ "marcusSystem" ];
}
