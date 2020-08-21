{ pkgs, ... }:

let
  pipeline.steps = [
    {
      key = "build-briefcase";
      command = "nix-build . -I briefcase=$(pwd) --no-out-link --show-trace";
      label = ":nix: build briefcase";
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
