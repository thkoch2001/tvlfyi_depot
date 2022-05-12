# Configuration for my Zerotier network.

{
  environment.persistence."/persist".directories = [
    "/var/lib/zerotier-one"
  ];

  services.zerotierone.enable = true;
  services.zerotierone.joinNetworks = [
    "35c192ce9bd4c8c7"
  ];

  networking.firewall.trustedInterfaces = [ "zt7nnembs4" ];
}
