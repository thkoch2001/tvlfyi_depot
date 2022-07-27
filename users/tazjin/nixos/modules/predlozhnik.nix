# Host predlozhnik.ru, serving //users/tazjin/predlozhnik
{ depot, ... }:

{
  services.nginx.virtualHosts."predlozhnik.ru" = {
    root = depot.users.tazjin.predlozhnik;
    enableACME = true;
    forceSSL = true;
  };
}
