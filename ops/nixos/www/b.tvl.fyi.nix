{ config, ... }:

{
  imports = [
    ./base.nix
  ];

  config = {
    services.nginx.virtualHosts."b.tvl.fyi" = {
      serverName = "b.tvl.fyi";
      serverAliases = [ "b.tvl.su" ];
      enableACME = true;
      forceSSL = true;

      extraConfig = ''
        location / {
          proxy_pass http://localhost:${toString config.services.depot.panettone.port};
        }
      '';
    };
  };
}
