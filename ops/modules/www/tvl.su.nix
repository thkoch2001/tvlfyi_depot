{ depot, ... }:

{
  imports = [
    ./base.nix
  ];

  config = {
    services.nginx.virtualHosts."tvl.su" = {
      serverName = "tvl.su";
      enableACME = true;
      forceSSL = true;

      extraConfig = ''
        add_header Strict-Transport-Security "max-age=31536000; includeSubDomains; preload" always;
      '';

      locations."/".root = depot.corp.website;
      locations."/tvixbolt".alias = depot.corp.tvixbolt;
    };
  };
}
