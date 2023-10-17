{ depot, pkgs, ... }:

let
  regenerate = pkgs.writeShellScriptBin "regenerate" ''
    (cd $(git rev-parse --show-toplevel)/tvix/castore-go && rm *.pb.go && cp ${depot.tvix.castore.protos.go-bindings}/*.pb.go . && chmod +w *.pb.go)
  '';
in
(pkgs.buildGoModule {
  name = "castore-go";
  src = depot.third_party.gitignoreSource ./.;
  vendorHash = "sha256-ZNtSSW+oCxMsBtURSrea9/GyUHDagtGefM+Ii+VkgCA=";
}).overrideAttrs (_: {
  meta.ci.extraSteps = {
    check = {
      label = ":water_buffalo: ensure generated protobuf files match";
      needsOutput = true;
      command = pkgs.writeShellScript "pb-go-check" ''
        ${regenerate}
        if [[ -n "$(git status --porcelain -unormal)" ]]; then
            echo "-----------------------------"
            echo ".pb.go files need to be updated, mg run //tvix/castore-go/regenerate"
            echo "-----------------------------"
            git status -unormal
            exit 1
        fi
      '';
      alwaysRun = true;
    };
  };
  passthru.regenerate = regenerate;
})
