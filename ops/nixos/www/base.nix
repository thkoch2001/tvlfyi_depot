{ config, ... }:

{
  config = {
    services.nginx = {
      enable = true;
      enableReload = true;

      recommendedTlsSettings = true;
      recommendedGzipSettings = true;
      recommendedProxySettings = true;
    };
  };
}
