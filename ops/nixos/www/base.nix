{ config, pkgs, lib, ... }:

{
  config = {
    services.nginx = {
      enable = true;
      enableReload = true;

      # Set up for lua-resty-openidc.
      package = pkgs.openresty;
      appendHttpConfig = let
        extraPureLuaPackages = with pkgs.luajitPackages; [
          lua-resty-openidc
          lua-resty-http
          lua-resty-session
          lua-resty-jwt
        ];
        luaPath = pkg: "${pkg}/share/lua/5.1/?.lua";
        makeLuaPath = lib.concatMapStringsSep ";" luaPath;
      in ''
        lua_package_path '${makeLuaPath extraPureLuaPackages};;';
        lua_ssl_trusted_certificate /etc/ssl/certs/ca-certificates.crt;
        lua_ssl_verify_depth 5;

        # cache for OIDC discovery metadata
        lua_shared_dict discovery 1m;
        # cache for JWKs.
        lua_shared_dict jwks 1m;
      '';
      resolver.addresses = [ "[2001:4860:4860::8888]" "8.8.8.8" ];

      recommendedTlsSettings = true;
      recommendedGzipSettings = true;
      recommendedProxySettings = true;
    };

    systemd.services.nginx.serviceConfig = {
      MemoryDenyWriteExecute = lib.mkForce false;  # openresty; now with LuaJit
      EnvironmentFile = "/etc/secrets/nginx";
    };

    # NixOS 20.03 broke nginx and I can't be bothered to debug it
    # anymore, all solution attempts have failed, so here's a
    # brute-force fix.
    #
    # TODO(tazjin): Find a link to the upstream issue and see if
    # they've sorted it after ~20.09
    systemd.services.fix-nginx = {
      script = "${pkgs.coreutils}/bin/chown -f -R nginx: /var/spool/nginx /var/cache/nginx";

      serviceConfig = {
        User = "root";
        Type = "oneshot";
      };
    };

    systemd.timers.fix-nginx = {
      wantedBy = [ "multi-user.target" ];
      timerConfig = {
        OnCalendar = "minutely";
      };
    };
  };
}
