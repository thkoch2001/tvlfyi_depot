{ pkgs
, depot
, ...
}:
let
  inherit ( builtins ) path toJSON;
  pipeline.steps = [
    {
      key = "lint-secrets";
      command = "${ pkgs.git-secrets }/bin/git-secrets --scan-history";
      label = ":broom: lint secrets";
    }
  ];
in
pkgs.writeText "pipeline.yaml" ( toJSON pipeline )
