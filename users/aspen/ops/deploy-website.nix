{ depot, pkgs, ... }:
{ domain, bucket ? domain, passthru ? { } }:
site:
with pkgs;
let
  website = depot.nix.writeTree domain site;

  purge-cf = writeShellApplication {
    name = "purge-cf.sh";
    runtimeInputs = [ httpie jq pass ];
    text = ''
      cfapi() {
        http \
          "https://api.cloudflare.com/client/v4/$1" \
          X-Auth-Email:root@gws.fyi \
          "X-Auth-Key: $(pass cloudflare-api-key)" \
          "''${@:2}"
      }

      zone_id=$(
        cfapi zones \
          | jq -r '.result[] | select(.name == "${domain}") | .id'
      )

      cfapi "zones/$zone_id/purge_cache" purge_everything:=true
    '';
  };
in
(writeShellApplication {
  name = "deploy.sh";
  runtimeInputs = [ awscli2 ];
  text = ''
    aws --profile personal s3 sync ${website}/ ${bucket}
    echo "Deployed to http://${domain}"
  '';
}).overrideAttrs { passthru = { inherit website purge-cf; } // passthru; }
