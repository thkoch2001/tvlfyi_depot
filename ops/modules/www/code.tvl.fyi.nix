{ depot, config, ... }:

{
  imports = [ ./base.nix ];

  config = {
    services.nginx.virtualHosts.cgit = {
      serverName = "code.tvl.fyi";
      serverAliases = [ "code.tvl.su" ];
      enableACME = true;
      forceSSL = true;

      extraConfig = ''
        # Serve the rendered Tvix component SVG.
        #
        # TODO(tazjin): Implement a way of serving this dynamically
        location = /about/tvix/docs/component-flow.svg {
            alias ${depot.tvix.docs.svg}/component-flow.svg;
        }

        # Git operations on depot.git hit josh
        location /depot.git {
            proxy_pass http://localhost:${
              toString config.services.depot.git-serving.joshPort
            };
        }

        # Git clone operations on '/' should be redirected to josh now.
        location = /info/refs {
            return 302 https://code.tvl.fyi/depot.git/info/refs$is_args$args;
        }

        # Static assets must always hit the root.
        location ~ ^/(favicon\.ico|cgit\.(css|png))$ {
           proxy_pass http://localhost:2448;
        }

        # Everything else is forwarded to cgit for the web view
        location / {
            proxy_pass http://localhost:2448/cgit.cgi/depot/;
        }
      '';
    };
  };
}
