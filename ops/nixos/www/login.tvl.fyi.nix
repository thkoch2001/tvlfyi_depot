{ ... }:

{
  imports = [
    ./base.nix
  ];

  config = {
    services.nginx.virtualHosts."login.tvl.fyi" = {
      serverName = "login.tvl.fyi";
      forceSSL = true;

      extraConfig = ''
        location / {
          proxy_pass http://localhost:8443;
          proxy_set_header X-Forwarded-For $remote_addr;
          proxy_set_header Host $host;
        }
      '';
    };
  };
}
