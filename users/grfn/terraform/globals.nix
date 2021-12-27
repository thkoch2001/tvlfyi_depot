{ pkgs, ... }:

{
  provider.aws = map (region: {
    inherit region;
    alias = region;
    profile = "personal";
  }) [
    "us-east-1"
    "us-east-2"
    "us-west-2"
  ];

  data.external.cloudflare_api_key = {
    program = [(pkgs.writeShellScript "cloudflare_api_key" ''
      jq -n --arg api_key "$(pass cloudflare-api-key)" '{"api_key":$api_key}'
    '')];
  };

  provider.cloudflare = {
    email = "root@gws.fyi";
    api_key = "\${data.external.cloudflare_api_key.result.api_key}";
  };
}
