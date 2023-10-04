{ config, lib, pkgs, depot, ... }:

{
  imports = [
    # Third party modules we use
    "${depot.third_party.agenix.src}/modules/age.nix"
    # These modules touch things related to booting (filesystems, initrd network…)
    ./hardware.nix
    ./network.nix
    # These modules configure services, websites etc.
    ../../modules/disk-checkup.nix
    ./minecraft.nix
    ./gopher.nix
    ./http/sterni.lv.nix
    ./http/code.sterni.lv.nix
    ./http/flipdot.openlab-augsburg.de.nix
    ./http/likely-music.sterni.lv.nix
  ];

  config = {
    time.timeZone = "Europe/Berlin";

    nix = {
      package = pkgs.nix_2_3;
      settings = {
        trusted-public-keys = lib.mkAfter [
          "headcounter.org:/7YANMvnQnyvcVB6rgFTdb8p5LG1OTXaO+21CaOSBzg="
        ];
        substituters = lib.mkAfter [
          "https://hydra.build"
        ];
      };
    };
    tvl.cache.enable = true;

    services = {
      journald.extraConfig = ''
        SystemMaxUse=1024M
      '';

      openssh.enable = true;
    };

    security.acme = {
      defaults.email = builtins.getAttr "email" (
        builtins.head (
          builtins.filter (attrs: attrs.username == "sterni") depot.ops.users
        )
      );
      acceptTerms = true;
    };

    programs = {
      fish.enable = true;
      mosh.enable = true;
      tmux.enable = true;
    };

    environment.systemPackages = [
      pkgs.weechat
      pkgs.wget
      pkgs.git
      pkgs.stow
      pkgs.htop
      pkgs.foot.terminfo
      pkgs.vim
    ];

    users = {
      users = {
        root.openssh.authorizedKeys.keys = depot.users.sterni.keys.all;
        lukas = {
          isNormalUser = true;
          extraGroups = [ "wheel" "http" "git" ];
          openssh.authorizedKeys.keys = depot.users.sterni.keys.all;
          shell = "${pkgs.fish}/bin/fish";
        };
      };
    };

    nix.settings.trusted-users = [
      "lukas"
    ];

    system.stateVersion = "20.09";
  };
}
