{ depot, pkgs, config, ... }:

{
  imports = [
    ./base.nix
  ];

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

        location = /go-get/tvix/castore-go {
            alias ${pkgs.writeText "go-import-metadata.html" ''<html><meta name="go-import" content="code.tvl.fyi/tvix/castore-go git https://code.tvl.fyi/depot.git:/tvix/castore-go.git"></html>''};
        }

        location = /go-get/tvix/store-go {
            alias ${pkgs.writeText "go-import-metadata.html" ''<html><meta name="go-import" content="code.tvl.fyi/tvix/store-go git https://code.tvl.fyi/depot.git:/tvix/store-go.git"></html>''};
        }

        location = /go-get/tvix/nar-bridge {
            alias ${pkgs.writeText "go-import-metadata.html" ''<html><meta name="go-import" content="code.tvl.fyi/tvix/nar-bridge git https://code.tvl.fyi/depot.git:/tvix/nar-bridge.git"></html>''};
        }

        location = /tvix/nar-bridge {
            if ($args ~* "/?go-get=1") {
                return 302 /go-get/tvix/nar-bridge;
            }
        }

        location = /tvix/castore-go {
            if ($args ~* "/?go-get=1") {
                return 302 /go-get/tvix/castore-go;
            }
        }

        location = /tvix/store-go {
            if ($args ~* "/?go-get=1") {
                return 302 /go-get/tvix/store-go;
            }
        }

        # Git operations on depot.git hit josh
        location /depot.git {
            proxy_pass http://127.0.0.1:${toString config.services.depot.josh.port};
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
