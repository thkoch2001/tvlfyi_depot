{ config, lib, pkgs, ... }:
let
  pkgs-unstable = import <nixpkgs-unstable> {};

  kubie = pkgs-unstable.kubie.overrideAttrs (drv: rec {
    name = "kubie-${version}";
    version = "0.8.4";
    src = pkgs.fetchFromGitHub {
      rev = "v${version}";
      owner = "sbstp";
      repo = "kubie";
      sha256 = "1f82xlhhxbjadjw609kr1kdm4n69c9mqjia4b3k505wjh7cc55n0";
    };

    cargoDeps = drv.cargoDeps.overrideAttrs (lib.const {
      name = "${name}-vendor.tar.gz";
      inherit src;
      outputHash = "0mish7wqwq5ynl98n6swdn5i6mg62aih5rfykbl3wx39b468n481";
    });
  });
in
{
  home.packages = with pkgs; [
    kubectl
    kubetail
    sops
    kubie
    # pkgs-unstable.argocd # provided by urbos
  ];

  programs.zsh.shellAliases = {
    "kc" = "kubectl";
    "kg" = "kc get";
    "kga" = "kc get --all-namespaces";
    "kpd" = "kubectl get pods";
    "kpa" = "kubectl get pods --all-namespaces";
    "klf" = "kubectl logs -f";
    "kdep" = "kubectl get deployments";
    "ked" =  "kubectl edit deployment";
    "kpw" = "kubectl get pods -w";
    "kew" = "kubectl get events -w";
    "kdel" = "kubectl delete";
    "knw" = "kubectl get nodes -w";
    "arsy" = "argocd app sync --prune";
  };

  home.file.".kube/kubie.yaml".text = ''
    shell: zsh
    prompt:
      zsh_use_rps1: true
  '';
}
