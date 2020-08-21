{ pkgs, ... }:

let
  pipeline.steps = [
    {
      command = ''
        nix-build '<nixpkgs/nixos>' \
          -I briefcase="$(pwd)" \
          -I nixpkgs=/var/lib/buildkite-agent-socrates/nixpkgs-channels \
          -I nixos-config=nixos/socrates/default.nix \
          -A system \
          --no-out-link \
          --show-trace
      '';
      label = ":hammer: build";
    }
  ];
in pkgs.writeText "socrates.yml" (builtins.toJSON pipeline)
