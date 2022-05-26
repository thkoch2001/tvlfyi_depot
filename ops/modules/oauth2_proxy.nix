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

  # Depend on the Keycloak service if it is running on the same
  # machine.
  depends_on = lib.optional config.services.keycloak.enable "keycloak.service";
in
{
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
      default = config.age.secretsDir + "/oauth2_proxy";
    };
  };

  config = lib.mkIf cfg.enable {
    systemd.services.oauth2_proxy = {
      inherit description;
      after = depends_on;
      wants = depends_on;
      wantedBy = [ "multi-user.target" ];

      serviceConfig = {
        Restart = "always";
        RestartSec = "5s";
        DynamicUser = true;
        EnvironmentFile = cfg.secretsFile;
        ExecStart = "${pkgs.oauth2_proxy}/bin/oauth2-proxy --config ${configFile}";
      };
    };
  };
}
