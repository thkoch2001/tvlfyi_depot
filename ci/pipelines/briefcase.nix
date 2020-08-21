{ pkgs, ... }:

let
  pipeline.steps = [
    {
      command = "${pkgs.git-secrets}/bin/git-secrets";
      label = ":briefcase: Briefcase [lint]";
    }
    {
      command = "nix-build . -I briefcase=$(pwd) --no-out-link --show-trace";
      label = ":briefcase: Briefcase [build]";
    }
  ];
in pkgs.writeText "briefcase.yaml" (builtins.toJSON pipeline)
