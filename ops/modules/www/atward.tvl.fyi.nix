# Serve atward, the query redirection ... thing.
{ config, ... }:

{
  imports = [
    ./base.nix
  ];

  config = {
    # Short link support (i.e. plain http://at) for users with a
    # configured tvl.fyi/tvl.su search domain.
    services.nginx.virtualHosts."at-shortlink" = {
      serverName = "at";
      extraConfig = "return 302 https://atward.tvl.fyi$request_uri;";
    };

    services.nginx.virtualHosts."atward" = {
      serverName = "atward.tvl.fyi";
      enableACME = true;
      forceSSL = true;

      serverAliases = [
        "atward.tvl.su"
        "at.tvl.fyi"
        "at.tvl.su"
      ];

      locations."/" = {
        proxyPass = "http://localhost:${toString config.services.depot.atward.port}";
      };
    };
  };
}
