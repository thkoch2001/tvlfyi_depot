{ pkgs, ... }:

let
  pipeline.steps = [
    {
      key = "lint";
      command = "${pkgs.git-secrets}/bin/git-secrets --scan-history";
      label = ":broom: lint";
    }
  ];
in pkgs.writeText "pipeline.yaml" (builtins.toJSON pipeline)
