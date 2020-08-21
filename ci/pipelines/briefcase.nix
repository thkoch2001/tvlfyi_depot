{ pkgs, ... }:

let
  pipeline.steps = [
    {
      key = "lint";
      command = "${pkgs.git-secrets}/bin/git-secrets --scan-history";
      label = ":broom: lint";
    }
    {
      key = "build";
      command = "nix-build . -I briefcase=$(pwd) --no-out-link --show-trace";
      label = ":nix: build";
      depends_on = "lint";
    }
  ];
in pkgs.writeText "briefcase.yaml" (builtins.toJSON pipeline)
