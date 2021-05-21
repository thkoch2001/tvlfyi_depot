{ ... }:

{
  imports = [
    ./base.nix
  ];

  config = {
    services.nginx.virtualHosts."login.tvl.fyi" = {
      serverName = "login.tvl.fyi";
      enableACME = true;
      forceSSL = true;

      extraConfig = ''
        location / {
          proxy_pass http://localhost:8444;
          proxy_set_header X-Forwarded-For $remote_addr;
          proxy_set_header X-Forwarded-Proto https;
          proxy_set_header Host $host;
        }
      '';
    };
  };
}
