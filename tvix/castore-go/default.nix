{ depot, pkgs, ... }:

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
        ${depot.tvix.castore-go-generate}
        if [[ -n "$(git status --porcelain -unormal)" ]]; then
            echo "-----------------------------"
            echo ".pb.go files need to be updated, run //tvix:castore-go-generate"
            echo "-----------------------------"
            git status -unormal
            exit 1
        fi
      '';
      alwaysRun = true;
    };
  };
})
