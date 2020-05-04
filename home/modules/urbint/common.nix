{ config, lib, pkgs, ... }:

with lib;

let
  aliases = {
    cluster = "gcloud container clusters get-credentials";
    vpon = "sudo systemctl start openvpn-urbint.service";
  };
in
{
  imports = [ ../lib/cloneRepo.nix ];

  options = {
    urbint.projectPath = mkOption {
      description = ''
      Root path to clone Urbint repos in, relative to your home directory
      '';
      type = types.str;
      default = "code";
    };
  };

  config = {
    services.lorri.enable = true;

    programs.direnv = {
      enable = true;
      enableBashIntegration = true;
      enableZshIntegration = true;
    };

    home.packages = with pkgs; [
      docker
      docker-compose
      skopeo

      kubectl
      kubetail
      google-cloud-sdk
      openvpn
      sops
      (import <nixpkgs-unstable> {}).argocd
      drone-cli

      git

      zoom-us
      slack
      _1password
    ];

    nixpkgs.config.allowUnfree = true;

    impure.clonedRepos = {
      grid = {
        github = "urbint/grid";
        path = "${config.urbint.projectPath}/grid";
      };

      docker-images = {
        github = "urbint/docker-images";
        path = "${config.urbint.projectPath}/docker-images";
      };

      gitops = {
        github = "urbint/gitops";
        path = "${config.urbint.projectPath}/gitops";
      };
    };

    programs.zsh.shellAliases = aliases;
    programs.bash.shellAliases = aliases;

    programs.zsh.envExtra = ''
      export DRONE_SERVER=https://ci.urbinternal.com
    '';
  };
}
