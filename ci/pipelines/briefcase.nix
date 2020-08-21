{ pkgs, ... }:

let
  pipeline.steps = [
    {
      command = ''
        echo $(pwd)
        git show HEAD
        git log -n 1
        ${pkgs.git-secrets}/bin/git-secrets --scan-history
      '';
      label = ":briefcase: Briefcase [lint]";
    }
    {
      command = "nix-build . -I briefcase=$(pwd) --no-out-link --show-trace";
      label = ":briefcase: Briefcase [build]";
    }
  ];
in pkgs.writeText "briefcase.yaml" (builtins.toJSON pipeline)
