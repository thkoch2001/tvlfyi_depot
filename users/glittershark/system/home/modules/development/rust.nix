{ config, lib, pkgs, ... }:


{
  home.packages = with pkgs; [
    rustup
    rust-analyzer
    cargo-edit
  ];

  programs.zsh.shellAliases = {
    "cg" = "cargo";
    "cb" = "cargo build";
    "ct" = "cargo test";
  };
}
