{ config, ... }:

{
  imports = [
    ./base.nix
  ];

  config = {
    services.nginx.virtualHosts.cgit = {
      serverName = "code.tvl.fyi";
      enableACME = true;
      forceSSL = true;

      extraConfig = ''
        # Static assets must always hit the root.
        location ~ ^/(favicon\.ico|cgit\.(css|png))$ {
           proxy_pass http://localhost:2448;
        }

        # Serve the rendered Tvix component SVG.
        #
        # TODO(tazjin): Implement a way of serving this dynamically
        location =  /about/tvix/docs/component-flow.svg {
            try_files ${config.depot.tvix.docs.svg} =404;
        }

        # Everything else hits the depot directly.
        location / {
            proxy_pass http://localhost:2448/cgit.cgi/depot/;
        }
      '';
    };
  };
}
