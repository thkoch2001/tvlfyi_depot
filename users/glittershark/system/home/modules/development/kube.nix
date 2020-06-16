{ config, lib, pkgs, ... }:
let
  pkgs-unstable = import <nixpkgs-unstable> {};
in
{
  home.packages = with pkgs; [
    kubectl
    kubetail
    sops
    pkgs-unstable.kubie
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
    "kev" = "kubectl get events --sort-by='.metadata.creationTimestamp'";

    "arsy" = "argocd app sync --prune";
  };

  home.file.".kube/kubie.yaml".text = ''
    shell: zsh
    prompt:
      zsh_use_rps1: true
  '';
}
