# Configuration for the TVL buildkite agents.
{ config, depot, lib, ... }:

let
  cfg = config.services.depot.buildkite;
  agents = lib.range 1 cfg.agentCount;
  description = "Buildkite agents for TVL";

  # All Buildkite hooks are actually besadii, but it's being invoked
  # with different names.
  buildkiteHooks = depot.third_party.runCommandNoCC "buildkite-hooks" {} ''
    mkdir -p $out/bin
    ln -s ${depot.ops.besadii}/bin/besadii $out/bin/post-command
  '';
in {
  options.services.depot.buildkite = {
    enable = lib.mkEnableOption description;
    agentCount = lib.mkOption {
      type = lib.types.int;
      description = "Number of Buildkite agents to launch";
    };
  };

  config = lib.mkIf cfg.enable {
    # Run the Buildkite agents using the default upstream module.
    services.buildkite-agents = builtins.listToAttrs (map (n: rec {
      name = "whitby-${toString n}";
      value = {
        inherit name;
        enable = true;
        tokenPath = "/etc/secrets/buildkite-agent-token";
        hooks.post-command = "${buildkiteHooks}/bin/post-command";
      };
    }) agents);
  };

  # Set up a group for all Buildkite agent users
  users = {
    groups.buildkite-agents = {};
    users = listToAttrs (map (n: {
      name = "buildkite-agent-whitby-${toString n}";
      value.extraGroups = [ "buildkite-agents" ];
    }) (range 1 32));
  };
}
