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

        location /rust {
          root ${depot.tvix.rust-docs};
        }
      '';
    };
  };
}
