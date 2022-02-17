# Redirect whitby.tvl.fyi to the machine configuration.

{
  imports = [
    ./base.nix
  ];

  config.services.nginx.virtualHosts."whitby.tvl.fyi" = {
    serverName = "whitby.tvl.fyi";
    serverAliases = [ "whitby.tvl.su" ];
    enableACME = true;

    extraConfig = ''
      return 302 https://at.tvl.fyi/?q=%2F%2Fops%2Fmachines%2Fwhitby;
    '';
  };
}
