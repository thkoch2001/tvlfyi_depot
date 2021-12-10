# Configuration for the TVL buildkite agents.
{ config, depot, pkgs, lib, ... }:

let
  cfg = config.services.depot.buildkite;
  agents = lib.range 1 cfg.agentCount;
  description = "Buildkite agents for TVL";

  besadiiWithConfig = name: pkgs.writeShellScript "besadii-whitby" ''
    export BESADII_CONFIG=/etc/secrets/besadii.json
    exec -a ${name} ${depot.ops.besadii}/bin/besadii "$@"
  '';

  # All Buildkite hooks are actually besadii, but it's being invoked
  # with different names.
  buildkiteHooks = pkgs.runCommandNoCC "buildkite-hooks" {} ''
    mkdir -p $out/bin
    ln -s ${besadiiWithConfig "post-command"} $out/bin/post-command
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
        tokenPath = "/run/agenix/buildkite-agent-token";
        runtimePackages = with pkgs; [ curl jq ];
        hooks.post-command = "${buildkiteHooks}/bin/post-command";
      };
    }) agents);

    # Set up a group for all Buildkite agent users
    users = {
      groups.buildkite-agents = {};
      users = builtins.listToAttrs (map (n: rec {
        name = "buildkite-agent-whitby-${toString n}";
        value = {
          isSystemUser = true;
          group = lib.mkForce "buildkite-agents";
          extraGroups = [ name ];
        };
      }) agents);
    };
  };
}
