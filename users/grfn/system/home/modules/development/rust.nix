{ config, lib, pkgs, ... }:


{
  home.packages = with pkgs; [
    rustup
    rust-analyzer
    cargo-edit
    cargo-expand
    cargo-flamegraph
    cargo-rr
    cargo-udeps
    sccache
    evcxr
  ];

  programs.zsh.shellAliases = {
    "cg" = "cargo";
    "cb" = "cargo build";
    "ct" = "cargo test";
    "ctw" = "fd -e rs | entr cargo test";
    "cch" = "cargo check";
  };

  home.file.".cargo/config".text = ''
    [build]
    rustc-wrapper = "${pkgs.sccache}/bin/sccache"
  '';
}
