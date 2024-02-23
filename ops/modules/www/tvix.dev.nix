{ depot, ... }:

{
  imports = [
    ./base.nix
  ];

  config = {
    services.nginx.virtualHosts."tvix.dev" = {
      serverName = "tvix.dev";
      enableACME = true;
      forceSSL = true;
      root = depot.tvix.website;

      extraConfig = ''
        add_header Strict-Transport-Security "max-age=31536000; includeSubDomains; preload" always;
      '';
    };

    services.nginx.virtualHosts."bolt.tvix.dev" = {
      root = depot.web.tvixbolt;
      enableACME = true;
      forceSSL = true;

      extraConfig = ''
        add_header Strict-Transport-Security "max-age=31536000; includeSubDomains; preload" always;
      '';
    };
  };

  # old domain, serve redirect
  services.nginx.virtualHosts."tvixbolt.tvl.su" = {
    enableACME = true;
    forceSSL = true;
    extraConfig = "return 301 https://bolt.tvix.dev$request_uri;";
  };

  services.nginx.virtualHosts."docs.tvix.dev" = {
    serverName = "docs.tvix.dev";
    enableACME = true;
    forceSSL = true;

    extraConfig = ''
      add_header Strict-Transport-Security "max-age=31536000; includeSubDomains; preload" always;

      location = / {
        # until we have a better default page here
        return 301 https://docs.tvix.dev/rust/tvix_eval/index.html;
      }

      location /rust/ {
        alias ${depot.tvix.rust-docs}/;
      }
    '';
  };
}
