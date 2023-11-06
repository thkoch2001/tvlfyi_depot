args@{ pkgs, depot, ... }:
with pkgs;
let
  site = import ./site.nix args;
  resume = import ../resume args;
  bucket = "s3://gws.fyi";
  distributionID = "E2ST43JNBH8C64";

  css = runCommand "main.css"
    {
      buildInputs = [ pkgs.minify ];
    } ''
    minify --type css < ${./main.css} > $out
  '';

  keys = runCommand "ssh-keys" { } ''
    touch $out
    echo "${depot.users.grfn.keys.main}" >> $out
  '';

  website =
    runCommand "gws.fyi" { } ''
      mkdir -p $out
      cp ${css} $out/main.css
      cp ${site.index} $out/index.html
      cp -r ${site.recipes} $out/recipes
      cp ${resume} $out/resume.pdf
      cp ${keys} $out/keys
    '';

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
          | jq -r '.result[] | select(.name == "gws.fyi") | .id'
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
    echo "Deployed to http://gws.fyi"
  '';
}).overrideAttrs {
  passthru = { inherit website site purge-cf; };
}
