# Configuration for the coordination server for net.tvl.fyi, a
# tailscale network run using headscale.
#
# All TVL members can join this network, which provides several exit
# nodes through which traffic can be routed.
#
# The coordination server is currently run on sanduny.tvl.su. It is
# managed manually, ping somebody with access ... for access.
#
# Servers should join using approximately this command:
#   tailscale up --login-server https://net.tvl.fyi --accept-dns=false --advertise-exit-node
#
# Clients should join using approximately this command:
#   tailscale up --login-server https://net.tvl.fyi --accept-dns=false
{ config, pkgs, ... }:

{
  # TODO(tazjin): run embedded DERP server
  services.headscale = {
    enable = true;
    port = 4725; # hscl

    settings = {
      server_url = "https://net.tvl.fyi";
      dns_config.nameservers = [
        "8.8.8.8"
        "1.1.1.1"
        "77.88.8.8"
      ];

      # TLS is handled by nginx
      tls_cert_path = null;
      tls_key_path = null;
    };
  };

  environment.systemPackages = [ pkgs.headscale ]; # admin CLI

  services.nginx.virtualHosts."net.tvl.fyi" = {
    serverName = "net.tvl.fyi";
    enableACME = true;
    forceSSL = true;

    # See https://github.com/juanfont/headscale/blob/v0.22.3/docs/reverse-proxy.md#nginx
    extraConfig = ''
      location / {
        proxy_pass http://localhost:${toString config.services.headscale.port};
        proxy_http_version 1.1;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection $connection_upgrade;
        proxy_set_header Host $server_name;
        proxy_redirect http:// https://;
        proxy_buffering off;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $http_x_forwarded_proto;
      }
    '';
  };

}
