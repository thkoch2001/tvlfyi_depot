# Module that configures Buildkite agents on TVL build machines.
{ config, lib, pkgs, ... }:

let
  inherit (lib) mkEnableOption mkIf mkOption types;
  cfg = config.services.depot.buildkite;
in {
  options.services.depot.buildkite = {
    enable = mkEnableOption "Enable TVL Buildkite agents";

    agents = mkOption {
      type = types.int;
      description = "Number of Buildkite agents";
    };
  };

  config = mkIf cfg.enable {
    # Run a handful of Buildkite agents
    services.buildkite-agents = listToAttrs (map (n: rec {
      name = "whitby-${toString n}";
      value = {
        inherit name;
        enable = true;
        tokenPath = "/etc/secrets/buildkite-agent-token";
        hooks.post-command = "${buildkiteHooks}/bin/post-command";
      };
    }) (range 1 32));

  };
}
