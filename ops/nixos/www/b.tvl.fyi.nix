{ config, ... }:

{
  imports = [
    ./base.nix
  ];

  config = {
    services.nginx.virtualHosts."b.tvl.fyi" = {
      serverName = "b.tvl.fyi";
      serverAliases = [ "b.tvl.su" ];
      enableACME = true;
      forceSSL = true;

      extraConfig = ''
        location / {
          proxy_pass http://localhost:${toString config.services.depot.panettone.port};

          access_by_lua_block {
            local opts = {
              redirect_uri = "/loginz",
              discovery = "https://login.tvl.fyi/oidc/.well-known/openid-configuration",
              client_id = "OAUTH-TVL-panettone-iNGNPe3v3l",
              client_secret = os.getenv("PANETTONE_CLIENT_SECRET"),
              scope = "openid",
              accept_none_alg = false,
              accept_unsupported_alg = false,
              revoke_tokens_on_logout = true,
            }

            -- check session, but if not logged in don't force it
            local auth_mode = "pass"
            local is_login = ngx.var.uri == "/login"
            if is_login then
              -- ...unless we're hitting /login, then log us in
              auth_mode = nil
            end

            local res, err = require("resty.openidc").authenticate(opts, nil, auth_mode)
            if err then
              ngx.status = 500
              ngx.say(err)
              ngx.exit(ngx.HTTP_INTERNAL_SERVER_ERROR)
            end

            if is_login then
              -- we made it! now redirect to the requested path.
              local args, err = ngx.req.get_uri_args()
              if err then
                ngx.status = 500
                ngx.say(err)
                ngx.exit(ngx.HTTP_INTERNAL_SERVER_ERROR)
              end

              local original_url = args["original-uri"]
              if not ngx.re.match(original_url, "^/") then
                original_url = "/"
              end

              return ngx.redirect(original_url)
            end

            if res and res.id_token and res.id_token.sub then
              ngx.req.set_header("Authenticated-User", res.id_token.sub)
              ngx.req.set_header("Authenticated-User-DN", "cn="..res.id_token.sub..",ou=users,dc=tvl,dc=fyi")
            end
          }
        }
      '';
    };
    services.nginx.appendConfig = ''
      env PANETTONE_CLIENT_SECRET;
    '';
  };
}
