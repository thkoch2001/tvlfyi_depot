# Host predlozhnik.ru, serving //users/tazjin/predlozhnik
{ depot, ... }:

{
  services.nginx.virtualHosts."predlozhnik.ru" = {
    root = depot.corp.russian.predlozhnik;
    enableACME = true;
    forceSSL = true;
  };
}
