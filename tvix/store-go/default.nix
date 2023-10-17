{ depot, pkgs, ... }:

(pkgs.buildGoModule {
  name = "store-go";
  src = depot.third_party.gitignoreSource ./.;
  vendorHash = "sha256-WAYaIT3h3Cdvo1RB8T7DuoxeKvXfkq8vo/vdkhJQDs0=";
}).overrideAttrs (_: {
  meta.ci.extraSteps = {
    check = {
      label = ":water_buffalo: ensure generated protobuf files match";
      needsOutput = true;
      command = pkgs.writeShellScript "pb-go-check" ''
        ${depot.tvix.store-go-generate}
        if [[ -n "$(git status --porcelain -unormal)" ]]; then
            echo "-----------------------------"
            echo ".pb.go files need to be updated, run //tvix:store-go-generate"
            echo "-----------------------------"
            git status -unormal
            exit 1
        fi
      '';
      alwaysRun = true;
    };
  };
})
