# Configuration for oauth2_proxy, which is used as a handler for nginx
# auth-request setups.
#
# This module exports a helper function at
# `config.services.depot.oauth2_proxy.withAuth` that can be wrapped
# around nginx server configuration blocks to configure their
# authentication setup.
{ config, depot, pkgs, lib, ... }:

let
  description = "OAuth2 proxy to authenticate TVL services";
  cfg = config.services.depot.oauth2_proxy;
  configFile = pkgs.writeText "oauth2_proxy.cfg" ''
    email_domains = [ "*" ]
    http_address = "127.0.0.1:${toString cfg.port}"
    provider = "keycloak-oidc"
    client_id = "oauth2-proxy"
    oidc_issuer_url = "https://auth.tvl.fyi/auth/realms/TVL"
    reverse_proxy = true
    set_xauthrequest = true
  '';

  # We need to prevent people from (accidentally?) overriding the
  # withAuth function. This method makes overriding it very hard.
  sentinel = builtins.currentTime;
  withAuthChecked = x: if x == sentinel then sentinel else withAuth x;

  withAuth = {}: otherConfig: otherConfig // {};
in {
  options.services.depot.oauth2_proxy = {
    enable = lib.mkEnableOption description;

    port = lib.mkOption {
      description = "Port to listen on";
      type = lib.types.int;
      default = 2884; # "auth"
    };

    secretsFile = lib.mkOption {
      type = lib.types.str;
      description = "EnvironmentFile from which to load secrets";
      default = "/run/agenix/oauth2_proxy";
    };

    withAuth = lib.mkOption {
      description = "Helper function to configure authentication";
      default = withAuth;
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = lib.singleton {
      # Note that this depends on Nix' weird pointer equality checks.
      assertion = sentinel == cfg.withAuth sentinel;
      message = "Must not override oauth2_proxy.withAuth!";
    };

    systemd.services.oauth2_proxy2 = {
      inherit description;
      wantedBy = [ "multi-user.target" ];

      serviceConfig = {
        Restart = "always";
        DynamicUser = true;
        EnvironmentFile = cfg.secretsFile;
        ExecStart = "${pkgs.oauth2_proxy}/bin/oauth2-proxy --config ${configFile}";
      };
    };
  };
}
