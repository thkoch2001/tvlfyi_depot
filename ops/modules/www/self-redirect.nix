# Redirect the hostname of a machine to its configuration in a web
# browser.
#
# Works by convention, assuming that the machine has its configuration
# at //ops/machines/${hostname}.
{ config, ... }:

let
  host = "${config.networking.hostName}.${config.networking.domain}";
in
{
  imports = [ ./base.nix ];

  config.services.nginx.virtualHosts."${host}" = {
    serverName = host;
    addSSL = true; # SSL is not forced on these redirects
    enableACME = true;

    extraConfig = ''
      location = / {
        return 302 https://at.tvl.fyi/?q=%2F%2Fops%2Fmachines%2F${config.networking.hostName};
      }
    '';
  };
}
