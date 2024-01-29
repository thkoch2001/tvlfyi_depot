{ config, lib, pkgs, ... }:

{
  home.packages = with pkgs; [
    ocaml

    # ocamlPackages.merlin
    # ocamlPackages.utop
    # ocamlPackages.ocp-indent
    # ocamlPackages.ocamlformat
  ];

  programs.opam = {
    enable = true;
    enableZshIntegration = true;
  };
}
