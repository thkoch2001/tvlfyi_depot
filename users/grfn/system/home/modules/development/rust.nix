{ config, lib, pkgs, ... }:

let
  inherit (config.lib) depot;
in

with lib;

{

  home.packages = with pkgs; [
    rustup
    cargo-edit
    cargo-expand
    cargo-udeps
    cargo-bloat
    sccache
    evcxr

    depot.users.grfn.pkgs.cargo-hakari
    depot.users.grfn.pkgs.cargo-nextest

    # benchmarking+profiling
    cargo-criterion
    cargo-flamegraph
    coz
    inferno
    hotspot
  ] ++ optionals (stdenv.isLinux) [
    cargo-rr
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

    [target.x86_64-unknown-linux-gnu]
    linker = "clang"
    rustflags = ["-C", "link-arg=-fuse-ld=${pkgs.mold}/bin/mold"]
  '';
}
