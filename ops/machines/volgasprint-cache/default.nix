# temporary machine for local binary cache proxy during VolgaSprint

{ depot, lib, pkgs, ... }: # readTree options
{ config, ... }: # passed by module system

let
  mod = name: depot.path.origSrc + ("/ops/modules/" + name);
in
{
  imports = [
    (mod "tvl-users.nix")
  ];

  boot = {
    kernelPackages = pkgs.linuxKernel.packages.linux_rpi4;
    initrd.availableKernelModules = [ "xhci_pci" "usbhid" "usb_storage" ];
    loader = {
      grub.enable = false;
      generic-extlinux-compatible.enable = true;
    };
  };

  console = {
    packages = [ pkgs.terminus_font ];
    font = "${pkgs.terminus_font}/share/consolefonts/ter-i22b.psf.gz";
  };

  fileSystems = {
    "/" = {
      device = "/dev/disk/by-label/NIXOS_SD";
      fsType = "ext4";
      options = [ "noatime" ];
    };
    "/var/public-nix-cache" = {
      device = "/dev/sda1";
      fsType = "ext4";
    };
  };

  networking = {
    firewall = {
      enable = true;
      allowedTCPPorts = [ 80 443 8098 ];
    };

    hostName = "cache";
    domain = "volgasprint.org";

    wireless = {
      enable = true;
      networks.VolgaSprint.psk = "nixos-unstable";
      interfaces = [ "wlan0" ];
    };

    wg-quick.interfaces = {
      wg0 = {
        address = [ "10.10.10.2/24" "fd42::1/128" ];
        dns = [ "1.1.1.1" ];
        privateKeyFile = "/etc/wireguard_private_key";

        peers = [
          {
            publicKey = "2MZzEGJzA3HrwkHf91TaKJEHwCNyVvsTLWoIYHrCxhY=";
            presharedKeyFile = "/etc/wireguard_preshared_key";
            allowedIPs = [ "0.0.0.0/0" "::/0" ];
            endpoint = "195.201.63.240:8098";
            persistentKeepalive = 15;
          }
        ];
      };
    };
  };

  environment.systemPackages = with pkgs; [
    neovim
  ];

  services.openssh.enable = true;

  services.nginx = {
    enable = true;
    recommendedGzipSettings = true;
    recommendedOptimisation = true;

    appendHttpConfig = ''
      proxy_cache_path /tmp/pkgcache levels=1:2 keys_zone=cachecache:100m max_size=20g inactive=365d use_temp_path=off;

      # Cache only success status codes; in particular we don't want to cache 404s.
      # See https://serverfault.com/a/690258/128321
      map $status $cache_header {
      200     "public";
      302     "public";
      default "no-cache";
      }
      access_log /var/log/nginx/access.log;
    '';

    virtualHosts."cache.volgasprint.org" = {

      locations."/" = {
        root = "/var/public-nix-cache";
        extraConfig = ''
          expires max;
          add_header Cache-Control $cache_header always;
          # Ask the upstream server if a file isn't available locally
          error_page 404 = @fallback;
        '';
      };

      extraConfig = ''
        # Using a variable for the upstream endpoint to ensure that it is
        # resolved at runtime as opposed to once when the config file is loaded
        # and then cached forever (we don't want that):
        # see https://tenzer.dk/nginx-with-dynamic-upstreams/
        # This fixes errors like
        #   nginx: [emerg] host not found in upstream "upstream.example.com"
        # when the upstream host is not reachable for a short time when
        # nginx is started.
        resolver 80.67.169.12; # fdn dns
        set $upstream_endpoint http://cache.nixos.org;
      '';

      locations."@fallback" = {
        proxyPass = "$upstream_endpoint";
        extraConfig = ''
          proxy_cache cachecache;
          proxy_cache_valid  200 302  60d;
          expires max;
          add_header Cache-Control $cache_header always;
        '';
      };

      # We always want to copy cache.nixos.org's nix-cache-info file,
      # and ignore our own, because `nix-push` by default generates one
      # without `Priority` field, and thus that file by default has priority
      # 50 (compared to cache.nixos.org's `Priority: 40`), which will make
      # download clients prefer `cache.nixos.org` over our binary cache.
      locations."= /nix-cache-info" = {
        # Note: This is duplicated with the `@fallback` above,
        # would be nicer if we could redirect to the @fallback instead.
        proxyPass = "$upstream_endpoint";
        extraConfig = ''
          proxy_cache cachecache;
          proxy_cache_valid  200 302  60d;
          expires max;
          add_header Cache-Control $cache_header always;
        '';
      };
    };
  };

  hardware.enableRedistributableFirmware = true;
  system.stateVersion = "23.11";
}
