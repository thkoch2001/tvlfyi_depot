{ pkgs, ... }:

let
  pipeline.steps = [
    {
      key = "lint-secrets";
      command = "${pkgs.git-secrets}/bin/git-secrets --scan-history";
      label = ":broom: lint secrets";
    }
    {
      key = "build-briefcase";
      command = "nix-build . -I briefcase=$(pwd) --no-out-link --show-trace";
      label = ":nix: build briefcase";
      depends_on = "lint-secrets";
    }
    {
      key = "build-socrates";
      command = ''
        nix-build '<nixpkgs/nixos>' \
          -I briefcase="$(pwd)" \
          -I nixpkgs=/var/lib/buildkite-agent-socrates/nixpkgs-channels \
          -I nixos-config=nixos/socrates/default.nix \
          -A system \
          --no-out-link \
          --show-trace
      '';
      label = ":nix: build socrates";
      depends_on = "build-briefcase";
    }
  ];
in pkgs.writeText "pipeline.yaml" (builtins.toJSON pipeline)
