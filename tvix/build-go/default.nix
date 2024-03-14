{ depot, pkgs, ... }:

let
  regenerate = pkgs.writeShellScript "regenerate" ''
    (cd $(git rev-parse --show-toplevel)/tvix/build-go && rm *.pb.go && cp ${depot.tvix.build.protos.go-bindings}/*.pb.go . && chmod +w *.pb.go)
  '';
in
(pkgs.buildGoModule {
  name = "build-go";
  src = depot.third_party.gitignoreSource ./.;
  vendorHash = "sha256-BprOPkgyT1F6TNToCN2uSHlkCXMdmv/QK+lTvA6O/rM=";
}).overrideAttrs
  (_: {
    meta.ci.extraSteps = {
      check = {
        label = ":water_buffalo: ensure generated protobuf files match";
        needsOutput = true;
        command = pkgs.writeShellScript "pb-go-check" ''
          ${regenerate}
          if [[ -n "$(git status --porcelain -unormal)" ]]; then
              echo "-----------------------------"
              echo ".pb.go files need to be updated, mg run //tvix/build-go/regenerate"
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
