{ depot, ... }:

let
  inherit (depot.users.grfn)
    terraform
  ;

in terraform.workspace "bbbg" {
  plugins = (p: with p; [
    aws
    cloudflare
  ]);
} {
  machine = terraform.nixosMachine {
    name = "bbbg";
    instanceType = "t3a.small";
    rootVolumeSizeGb = 250;
    extraIngressPorts = [ 80 443 ];
    configuration = { pkgs, lib, config, depot, ... }: {
      imports = [
        ./module.nix
        "${depot.third_party.agenix.src}/modules/age.nix"
      ];

      services.openssh.enable = true;

      services.nginx = {
        enable = true;
        recommendedTlsSettings = true;
        recommendedOptimisation = true;
        recommendedGzipSettings = true;
        recommendedProxySettings = true;
      };

      networking.firewall.enable = false;

      programs.zsh.enable = true;

      users.users.grfn = {
        isNormalUser = true;
        initialPassword = "password";
        extraGroups = [
          "wheel"
          "networkmanager"
          "audio"
          "docker"
        ];
        shell = pkgs.zsh;
        openssh.authorizedKeys.keys = [
          depot.users.grfn.keys.main
        ];
      };

      security.sudo.extraRules = [{
        groups = ["wheel"];
        commands = [{ command = "ALL"; options = ["NOPASSWD"]; }];
      }];

      nix.gc = {
        automatic = true;
        dates = "weekly";
        options = "--delete-older-than 30d";
      };

      age.secrets = {
        bbbg.file =
          depot.users.grfn.secrets."bbbg.age";
      };

      services.bbbg.enable = true;
      services.bbbg.database.enable = true;
      services.bbbg.proxy.enable = true;
      services.bbbg.domain = "bbbg.gws.fyi";

      security.acme.email = "root@gws.fyi";
      security.acme.acceptTerms = true;
    };
  };

  dns = {
    data.cloudflare_zone.gws-fyi = {
      name = "gws.fyi";
    };

    resource.cloudflare_record.bbbg = {
      zone_id = "\${data.cloudflare_zone.gws-fyi.id}";
      name = "bbbg";
      type = "A";
      value = "\${aws_instance.bbbg_machine.public_ip}";
      proxied = false;
    };
  };
}
