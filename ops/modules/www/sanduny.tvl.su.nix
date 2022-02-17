# Redirect sanduny.tvl.su to the machine configuration.

{
  imports = [
    ./base.nix
  ];

  config.services.nginx.virtualHosts."sanduny.tvl.su" = {
    serverName = "sanduny.tvl.su";
    enableACME = true;

    extraConfig = ''
      return 302 https://at.tvl.fyi/?q=%2F%2Fops%2Fmachines%2Fsanduny;
    '';
  };
}
