# Experimental configuration for manually Livegrep.
{ config, ... }:

{
  imports = [
    ./base.nix
  ];

  config = {
    # Short link support (i.e. plain http://at) for users with a
    # configured tvl.fyi/tvl.su search domain.
    services.nginx.virtualHosts."grep.tvl.fyi" = {
      enableACME = true;
      forceSSL = true;

      locations."/" = {
        # experimental: manually run Docker container
        proxyPass = "http://172.17.0.3:8910";
      };
    };
  };
}
