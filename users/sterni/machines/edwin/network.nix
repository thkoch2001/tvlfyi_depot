{ config, pkgs, lib, depot, ... }:

let
  ipv6 = "2a01:4f8:151:54d0::/64";

  ipv4 = "176.9.107.207";
  gatewayv4 = "176.9.107.193";
  netmaskv4 = "255.255.255.224";
in

{
  config = {
    boot = {
      kernelParams = [
        "ip=${ipv4}::${gatewayv4}:${netmaskv4}::eth0:none"
      ];

      initrd.network = {
        enable = true;
        ssh = {
          enable = true;
          authorizedKeys = depot.users.sterni.keys.all;
          hostKeys = [
            "/etc/nixos/unlock_rsa_key_openssh"
            "/etc/nixos/unlock_ed25519_key_openssh"
          ];
        };
        postCommands = ''
          echo 'cryptsetup-askpass' >> /root/.profile
        '';
      };
    };

    networking = {
      usePredictableInterfaceNames = false;
      useDHCP = false;
      interfaces."eth0".useDHCP = false;

      hostName = "edwin";

      firewall = {
        enable = true;
        allowPing = true;
        allowedTCPPorts = [ 22 80 443 ];
      };
    };

    systemd.network = {
      enable = true;
      networks."eth0".extraConfig = ''
        [Match]
        Name = eth0

        [Network]
        Address = ${ipv6}
        Gateway = fe80::1
        Address = ${ipv4}/27
        Gateway = ${gatewayv4}
      '';
    };
  };
}
