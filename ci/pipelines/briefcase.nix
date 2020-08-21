{ pkgs, ... }:

let
  pipeline.steps = [
    {
      command = "${pkgs.git-secrets}/bin/git-secrets --scan-history";
      label = ":broom: lint";
    }
    {
      command = "nix-build . -I briefcase=$(pwd) --no-out-link --show-trace";
      label = ":hammer: build";
    }
  ];
in pkgs.writeText "briefcase.yaml" (builtins.toJSON pipeline)
