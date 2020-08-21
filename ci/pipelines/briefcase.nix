{ pkgs, ... }:

let
  pipeline.steps = [
    {
      command = ''
        git log -n 1
        git show
        ${pkgs.git-secrets}/bin/git-secrets --scan-history
      '';
      label = ":broom: lint";
    }
    {
      command = "nix-build . -I briefcase=$(pwd) --no-out-link --show-trace";
      label = ":nix: build";
    }
  ];
in pkgs.writeText "briefcase.yaml" (builtins.toJSON pipeline)
